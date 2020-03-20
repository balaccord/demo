package site2_module;

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

# $self->parse_list($urlbase, $html)
sub parse_list {
  my($self, $urlbase, $url, $html) = @_;
  $self->log_trace(__LINE__, ' ');

  my $md = Mojo::DOM->new($html);

$DB::signal = $DB::signal = 1;

  # блок со списком квартир
  my $lodges = $md->find('div[id=listingResults] > div.b-results-tile');
  $lodges && $lodges->size > 0 || $self->log_parse_error_critical(__LINE__, $url, $html, "Список квартир пуст или ошибка парсинга");

  my $found_current = 1;

  # блок со счётчиком
  my $found_total = $md->at('h4.g-no-margins')
    or $self->log_parse_error_critical(__LINE__, $url, $html, "Не найден счётчик количества публикаций");
  $found_total &&= $found_total->text;
  $found_total =~ s/\D+//g;

  my $current_page = $md->at('ul.e-pages > li.active > a')
    or $self->log_parse_error_critical(__LINE__, $url, $html, "Не найден номер текущей страницы");
  $current_page &&= $current_page->text;

  $found_current = ($current_page-1) * 21 + 1; # примерное число, потому что платные объявления размещаются на каждой странице

  $self->log_info(__LINE__, "GET $found_current (APPROX.) OF $found_total ON PAGE $current_page");

  # ссылка на следующую страницу или пустое значение
  my $next_page_url = $md->at('div.b-pager > a:last-of-type')
    or $self->log_parse_error_critical(__LINE__, $url, $html, "Не найдена ссылка на следующую страницу поиска");
  $next_page_url = $next_page_url->attr('href');
  $next_page_url = ($next_page_url =~ '^javascript') ? '' : URI->new_abs($next_page_url, $urlbase)->as_string;

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
    $info{title} = $link->attr("aria-label");

    my $full_desc = $lodge->at('div[itemprop=description]')
      or $self->log_parse_error_critical(__LINE__, $url, $html, "Не найдено полное описание ссылки $info{url}");
    $info{full_desc} = $full_desc->text;

# только для незалогиненых пользователей

    $info{price} = $lodge->at('h2[itemprop=price]')
      or $self->log_parse_error_critical(__LINE__, $url, $html, "Не найдена цена в элементе $info{url}");
    $info{price} &&= $info{price}->text;
    $info{price} =~ s/\D//g;

    if(my $address = $lodge->at('span[itemprop=address]')) {
      $info{_address} = $address->content; #all_text;
    }

    if(my $square_storey = $lodge->at('div.e-tile-size')) {
      $info{_square_storey} = $square_storey->content; #all_text;
    }

    $self->dispatch(\%info);
    $self->log_debug(__LINE__, Dumper \%info);

    $found_current++;
  };

  my $sleep = 5 + rand(5);
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

  $$info{region} = 'Нью-Гемпширская область';
  $$info{city} = 'Нью-Гемпшир';
  $$info{district} = 'Малхолландский';
#  $$info{estate_id} = '';
#  $$info{region_id} = '';
#  $$info{site_id} = '';
#  $$info{city_id} = '';
#  $$info{district_id} = '';
#  $$info{person} = '';
#  $$info{street} = '';
#  $$info{street_no} = '';
#  $$info{house_type_id} = '';
#  $$info{house_type} = '';
#  $$info{flat_type_id} = '';
  ($$info{site_pub_id}) = $html =~ m!<strong>Номер в каталоге:</strong>\s+(\d+)!s;
#  $$info{title} = '';
#  $$info{full_desc} = '';
  ($$info{creation_time}) = $html =~ m!<strong>Дата публикации объявления:</strong>\s+(\S+)!s;
  ($$info{publication_time}) = $html =~ m!<strong>Дата обновления объявления:</strong>\s+(\S+)!s;
#  $$info{store_time} = '';
#  $$info{seen_time} = '';
#  $$info{url} = '';
  ($$info{total_square}) = $html =~ m!<strong>Площадь:</strong>\s+([\d,\.]+)!s;
  ($$info{living_square}) = $html =~ m!<strong>Жилая площадь.*?</strong>\s+([\d,\.]+)!s;
  ($$info{kitchen_square}) = $html =~ m!<strong>Площадь кухни.*?</strong>\s+([\d,\.]+)!s;
  ($$info{rooms_count}) = $html =~ m!<strong>Комнаты:</strong>\s+(\d+)!s;
#  ($$info{storeys}) = '';
  ($$info{storey}) = $html =~ m!<strong>Этаж:</strong>\s+(\S+)!s;
#  $$info{balcony} = '';
#  $$info{published} = '';
#  $$info{hidden} = '';
#  $$info{deleted} = '';
#  $$info{has_photos} = '';
#  $$info{bath_joint} = '';
#  $$info{other_info} = '';
#  $$info{debug_data} = '';
#  $$info{geo_lat} = '';
#  $$info{geo_lon} = '';
#  $$info{house_cond} = '';

  @{$info}{qw(storey storeys)} = ($$info{storey} =~ m!^(\d+)/(\d+)$!);

  $$info{full_desc} = $md->at('div.b-listing-details p.m-listing-description')
    or $self->log_parse_error_critical(__LINE__, $$info{url}, $html, "Не найдено полное описание");
  $$info{full_desc} = $$info{full_desc}->text;

  $$info{street} = $md->at('div.col-xs-8 div.row div.col-xs-12 p')
    or $self->log_parse_error_critical(__LINE__, $$info{url}, $html, "Не найден адрес");
  $$info{street} = $$info{street}->text;
  $$info{street} =~ s/, Малхолландский, Нью-Гемпшир.*//;

  my %tmp;

  my $ul_info = $md->at('div.b-listing-details div.b-details-table');
  $ul_info &&= $ul_info->children('li');
  $ul_info->each(sub {$_->content =~ m!strong>\s*(.+?):.*?</strong>\s*([^\r\n]+)!s; $tmp{$1} = $2; });

  my %tr = (
#    '' => 'balcony',
#    '' => 'bath_joint',
#    '' => 'city_id',
    'Дата публикации объявления' => 'creation_time',
#    '' => 'debug_data',
#    '' => 'deleted',
#    '' => 'district_id',
    'Номер в каталоге' => 'estate_id',
#    '' => 'flat_type_id',
#    '' => 'full_desc',
#    '' => 'geo_lat',
#    '' => 'geo_lon',
#    '' => 'has_photos',
#    '' => 'hidden',
#    '' => 'house_cond',
#    'Материал здания' => 'house_type',
    'Площадь кухни (м²)' => 'kitchen_square',
    'Жилая площадь (м²)' => 'living_square',
#    '' => 'other_info',
#    '' => 'person',
###    'Цена' => 'price',
    'Дата обновления объявления' => 'publication_time',
#    '' => 'published',
#    '' => 'region_id',
    'Комнаты' => 'rooms_count',
#    '' => 'seen_time',
#    '' => 'site_id',
#    '' => 'site_pub_id',
    'Этаж' => 'storey',
#    '' => 'storeys',
#    '' => 'store_time',
#    '' => 'street',
#    '' => 'street_no',
#    '' => 'title',
    'Площадь' => 'total_square',
#    '' => 'url',
  );
  
  my %tr_skip = ();

#  for(keys %tmp) {
#    if(exists $tr{$_}) {
#      $$info{$tr{$_}} = $tmp{$_};
#    } else {
#      $self->log_warning(__LINE__, "Неописанный параметр [PROPS] '$_' => '$tmp{$_}'")
#        unless exists $tr_skip{$_};
#    }
#  }
  %tmp = ();

  $self->log_debug(__LINE__, Dumper {'%info' => $info});
#exit(0);

  my $sleep = 15 + rand(15);
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

  $$info{person} ||= '';
#  $$info{street} ||= '';
  $$info{street_no} ||= '';
#  $$info{house_type_id} ||= 1;
#  $$info{flat_type_id} ||= 1;
  $$info{site_pub_id} ||= '';
#  $$info{title} ||= '';
#  $$info{full_desc} ||= '';
  $$info{creation_time} ||= '00:00:00 01-01-1970';
  $$info{publication_time} ||= '00:00:00 01-01-1970';
#  $$info{url} ||= '';
#  $$info{total_square} ||= '';
  $$info{living_square} ||= 0;
  $$info{kitchen_square} ||= 0;
  $$info{rooms_count} ||= 0;
  $$info{storeys} ||= 0;
  $$info{storey} ||= 0;
#  $$info{balcony} ||= 0;
#  $$info{has_photos} ||= 0;
  $$info{bath_joint} ||= 0;
  $$info{other_info} ||= '';
  $$info{debug_data} ||= '';

  $$info{balcony} ||= 0;
  $$info{seen_count} ||= 0;
  $$info{price} ||= 0;
  $$info{seen_count} ||= 0;

  $self->log_debug(__LINE__, Dumper {'%info' => $info});
#exit(0);
}

1;
