package site1_module;

use strict;
use warnings;
#use HTML::TreeBuilder;
#use HTML::Entities qw(decode_entities %entity2char);
use Data::Dumper;
use Encode;
use utf8;
use parsers;
use Mojo::DOM;
use Date::Calc qw(Today Add_Delta_Days);

use parent 'parsers';

sub site1_module_time_to_time {
  my $time = shift;

#$DB::signal = $DB::signal = 1;
  my %month = qw(
  	января		01
  	февраля		02
  	марта		03
  	апреля		04
  	мая		05
  	июня		06
  	июля		07
  	августа		08
  	сентября	09
  	октября		10
  	ноября		11
  	декабря		12
  );

  # формат "5 апреля в 20:14" или "5 апреля 20:14"
  my($year, $mon, $day) = Today();
  $mon =~ s/^(\d)$/0$1/;
  $day =~ s/^(\d)$/0$1/;

#$DB::signal = $DB::signal = 1;
  $time =~ s/^(\d+)\s+(\S+)\s+(?:в\s+)?(\d\d:\d\d)$/$3:00 $1-$month{$2}-$year/;

  # формат "сегодня в 20:14" или "Сегодня 20:14"
  $time =~ s/^[Сс]егодня\s+(?:в\s+)?(\d\d:\d\d)$/$1:00 $day-$mon-$year/;

  # формат "вчера в 20:14" или "Вчера 20:14"
  ($year, $mon, $day) = Add_Delta_Days(Today(), -1);
  $mon =~ s/^(\d)$/0$1/;
  $day =~ s/^(\d)$/0$1/;

  $time =~ s/^[Вв]чера\s+(?:в\s+)?(\d\d:\d\d)$/$1:00 $day-$mon-$year/;

  $time =~ s/^(\d\d:\d\d:\d\d)\s+(\d-\d\d-\d\d\d\d)$/$1 0$2/; # 00:00:00 0-00-0000
  $time =~ s/^(\d\d:\d\d:\d\d\s+\d\d)-(\d-\d\d\d\d)$/$1-0$2/; # 00:00:00 00-0-0000

  if($time =~ m!(\d+)\s+(\S+)\s+(\d+)!) { # 31 декабря 2017
#$DB::signal = $DB::signal = 1;
    my %mon = (
      'декабря'	=> 12,
    );
    if(defined $mon{$2}) {
      $time = "$1-$mon{$2}-$3 00:00:00";
    } else {
      die "Неверный формат времени: '$time'";
    }
  } elsif($time =~ m!(\d\d:\d\d:\d\d)\s(\d\d)-(\d\d)-(\d\d\d\d)!) {
    $time = "$4-$3-$2 $1"; # dd-mm-yyyy HH:MM:SS
  } else {
    die "Неверный формат времени: '$time'";
  }

  $time;
}

# $self->parse_list($urlbase, $html)
sub parse_list {
  my($self, $urlbase, $url, $html) = @_;
  $self->log_trace(__LINE__, ' ');

  # инициализируем парсер
  my $md = Mojo::DOM->new($html);

#$DB::signal = $DB::signal = 1;

  # блок со списком квартир
  my $lodges = $md->find('div[class^="item item_table"], div[class^="item item_gallery"], div.vip_item');
  $lodges && $lodges->size > 0 || $self->log_parse_error_critical(__LINE__, $url, $html, "Список квартир пуст или ошибка парсинга");

  my $found_current = 1;

  # блок со счётчиком
  my $found_total = $md->at('span.breadcrumbs-link-count')
    or $self->log_parse_error_critical(__LINE__, $url, $html, "Не найден счётчик количества публикаций");
  $found_total &&= $found_total->text;
  $found_total =~ s/\s//g;

  my $current_page = $md->at('span[class^="pagination-page pagination-page_current"]')
    or $self->log_parse_error_critical(__LINE__, $url, $html, "Не найден номер текущей страницы");
  $current_page &&= $current_page->text;

  $found_current = ($current_page-1) * 30 + 1; # примерное число, потому что платные объявления размещаются на каждой странице

  $self->log_info(__LINE__, "GET $found_current (APPROX.) OF $found_total ON PAGE $current_page");

  # ссылка на следующую страницу или пустое значение
  my $next_page_url = $md->at('a[class^="pagination-page js-pagination-next"]');
  $next_page_url &&= URI->new_abs($next_page_url->attr('href'), $urlbase)->as_string;

  $self->log_debug(__LINE__, Dumper [$current_page, $found_current, $found_total, $next_page_url]);
#exit(0);

  # цикл по элементам списка
  for(@{$lodges->to_array}) { # лучше, чем each(sub{}), потому что не пишет в лог "::__ANON__"

    my $lodge = Mojo::DOM->new($_->content);

    # инициализируем хеш данных, который будет потом везде передаваться
    my %info = (
      _found_current	=> $found_current,
      _found_total	=> $found_total,
    );

    my $link = $lodge->at('a')
      or $self->log_parse_error_critical(__LINE__, $url, $html, "Не найдена ссылка на описание");
    $info{url} = URI->new_abs($link->attr('href'), $urlbase)->as_string;
    $info{title0} = $link->attr("title");
    $info{title0} =~ s/Объявление\s+«(.+?)»/$1/;

# только для незалогиненых пользователей

#    $info{price} = $lodge->at('span[class="option price"]')
    $info{price} = $lodge->at('span[class="option price"],span[class="option c-2"]')
#    $info{price} = $lodge->at('span[class="price-value-popup-icon"]')
      or $self->log_parse_error_critical(__LINE__, $url, $html, "Не найдена цена в элементе $info{url}");

    $info{price} &&= $info{price}->text;
    $info{price} =~ s/\D//gs;
    $info{price} = undef unless $info{price};

    $info{publication_time0} = $lodge->at('div.created-date')
      or $self->log_parse_error_critical(__LINE__, $url, $html, "Не найдено время публикации в элементе $info{url}");
    $info{publication_time0} &&= $info{publication_time0}->text;
    $info{publication_time0} =~ s/(^\s+)|(\s+$)//g;
    $info{publication_time0} = site1_module_time_to_time($info{publication_time0});

    $self->dispatch(\%info);
    $self->log_debug(__LINE__, Dumper \%info);

    $found_current++;
  };
#exit(0);

  my $sleep = 3 + rand(3);
  $self->log_info(__LINE__, "SLEEP $sleep");
  sleep($sleep);

  $next_page_url;
}

# $self->parse_single(\%info)
sub parse_single {
  my($self, $info) = @_;
  $self->log_trace(__LINE__, ' ');
  $self->log_trace(__LINE__, Dumper {'%info' => $info});
  $self->log_info(__LINE__, "GET SINGLE $$info{url}");

  my $response = $$self{ua}->get($$info{url});
  unless($response->is_success) {
    $self->log_error(__LINE__, "GET ERROR ", $response->status_line);
    last;
  }
  my $html = $response->decoded_content;
  my $md = Mojo::DOM->new($html);

  my $addr_span = $md->at('div.item-map-location');
  $addr_span or $self->log_parse_error_critical(__LINE__, $$info{url}, $html, 'No div.item-map-location');
  #print Dumper [$addr_span];
  #print $addr_span->content;

  $$info{city} = $addr_span->at('meta[itemprop=addressLocality]');
  $$info{city} &&= $$info{city}->attr('content');
  $$info{city} =~ s/^\s+|\s+$//gs;

  $$info{district} = $addr_span->at('span[itemprop=address] > span');
  $$info{district} &&= $$info{district}->text;
  $$info{district} =~ s/^\s+|\s+$//gs;
  $$info{district} =~ s/\s*,$//;
  $$info{district} =~ s/^р-н\s+//;

  $$info{street} = $addr_span->at('span[itemprop=address] > span > span[itemprop=streetAddress]');
  $$info{street} &&= $$info{street}->text;
  $$info{street} =~ s/^\s+|\s+$//gs;

  # выделить дом из адреса
  $$info{street_no} = ($$info{street} =~ s/^(.+?)(?:\s+|\s*,\s*)(\d+\s*\S{1,3})$/$1/) ? $2 : '';

  $$info{street} =~ s/^ул\.\s*//;
  $$info{street} =~ s/^(?:пр\.|пр-т)\s*(.+)/$1 пр-т/;
  $$info{street} =~ s/^пер\.\s*(.+)/$1 пер./;

  @$info{qw(site_pub_id creation_time)} = $html =~ m!№\s+(\d+),\s+размещено\s+(.+?)\s*</div>!;

  my $ul_info = $md->at('div[class=item-view-block] > div[class=item-params] > ul');
  #print $ul_info->content;

  $$info{person} = $md->at('span.sticky-header-seller-text');
  $$info{person} &&= $$info{person}->attr('title');

  $$info{price} = $md->at('span.price-value-string');
  $$info{price} &&= $$info{price}->text;
#$DB::signal = $DB::signal = 1;
  $$info{price} =~ s/\D//gs;
  $$info{price} = undef unless $$info{price};

  #my $seller_info = $md->at('div.seller-info-prop > div.seller-info-label + div.seller-info-value');
  #print $seller_info->content;
  #print $seller_info->each(sub{ print $_->content });
  #->grep(qr/Контактное\s+лицо/);

  my %tmp;

  $ul_info &&= $ul_info->children('li');
  $ul_info->each(sub {$_->content =~ m!>\s*(.+?)\s*</span>\s*([^\r\n]+)!s; $tmp{$1} = $2; });

  my %tr = (
#    'Балкон/лоджия'                     => 'balcony',		# 'есть балкон'
#    'Дата последнего начала публикации' => 'publication_time',
    'Жилая площадь:'		=> 'living_square',
#    'Заголовок H1'                      => 'title',
#    'Имя владельца'                     => 'person',
    'Количество комнат:'	=> 'rooms_count',
#    'Количество просмотров'             => 'seen_count',
#    'Номер дома '			=> 'street_no',
    'Общая площадь:'		=> 'total_square',
    'Площадь кухни:'		=> 'kitchen_square',
#    'Район'                             => 'district',
#    'Регион'                            => 'region',
#    'Ремонт'                            => 'renew',		# 'Без ремонта', 'Типовой'
#    'Санузел'				=> 'bath_joint',
#    'Телефоны'                          => 'phones1',
#    'Тип дома:'			=> 'house_type',
#    'Улица'                             => 'street',
#    'Цена'                              => 'price',
    'Этаж:'			=> 'storey',
    'Этажей в доме:'		=> 'storeys',
  );

  my %tr_skip = ();

  for(keys %tmp) {
    if(exists $tr{$_}) {
      $$info{$tr{$_}} = $tmp{$_};
    } else {
      $self->log_warning(__LINE__, "Неописанный параметр [PROPS] '$_' => '$tmp{$_}'")
        unless exists $tr_skip{$_};
    }
  }
  %tmp = ();

  $$info{creation_time} = site1_module_time_to_time($$info{creation_time});

  for(qw(total_square kitchen_square living_square)) {
    exists $$info{$_} && $$info{$_} ne '' && $$info{$_} =~ s/^\s*(\d+).*$/$1/;
  }

  $$info{title} = $md->at('h1.title-info-title')->all_text;
  $$info{title} =~ s/^\s+|\s+$//gs;

  $$info{full_desc} = $md->at('div.item-description-text, div.item-description-html')->all_text;
  $$info{full_desc} =~ s/^\s+|\s+$//gs;

  $self->log_debug(__LINE__, Dumper {'%info' => $info});

  my $sleep = 10 + rand(10);
  $self->log_info(__LINE__, "SLEEP $sleep");
  sleep($sleep);
}

# приводим инфу в порядок
sub cleanup {
  my($self, $info) = @_;
$self->log_trace(__LINE__, ' ');

  my $dbh = $$self{dbh};

  $$info{region_id} = $self->region_id('Нью-Гемпширская область');
  delete $$info{region};

  $$info{city_id} = $self->city_id($$info{region_id}, $$info{city});
  delete $$info{city};

  $$info{district_id} = $self->district_id($$info{city_id}, $$info{district});
  delete $$info{district};

  delete $$info{title0};
  delete $$info{publication_time0};

  $$info{rooms_count} =~ s/^(\d+).+/$1/;

  $self->log_debug(__LINE__, Dumper {'%info' => $info});
#exit(0);
}

1;
