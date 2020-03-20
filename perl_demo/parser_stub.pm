package parser_stub;

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

  # блок со списком квартир
  my $lodges = $md->find('????????????');
  $lodges && $lodges->size > 0 || $self->log_parse_error_critical(__LINE__, $url, $html, "Список квартир пуст или ошибка парсинга");

  my $found_current = 1;

  # блок со счётчиком
  my $found_total = $md->at('?????????????');
    or $self->log_parse_error_critical(__LINE__, $url, $html, "Не найден счётчик количества публикаций");
  $found_total &&= $found_total->text;
  $found_total =~ s/\s//g;

  my $current_page = $md->at('???????????????????')
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

    $info{price} = $lodge->at('span[class="option price"]')
      or $self->log_parse_error_critical(__LINE__, $url, $html, "Не найдена цена в элементе $info{url}");
    $info{price} &&= $info{price}->text;
    $info{price} =~ s/[\s\D]//g;

    $info{publication_time0} = $lodge->at('div.created-date')
      or $self->log_parse_error_critical(__LINE__, $url, $html, "Не найдено время публикации в элементе $info{url}");
    $info{publication_time0} &&= $info{publication_time0}->text;
    $info{publication_time0} =~ s/(^\s+)|(\s+$)//g;
    $info{publication_time0} = avito_time_to_time($info{publication_time0});

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

  $$info{estate_id} = '';
  $$info{region_id} = '';
  $$info{site_id} = '';
  $$info{city_id} = '';
  $$info{district_id} = '';
  $$info{person} = '';
  $$info{street} = '';
  $$info{street_no} = '';
#  $$info{house_type_id} = '';
#  $$info{flat_type_id} = '';
  $$info{site_pub_id} = '';
  $$info{title} = '';
  $$info{full_desc} = '';
  $$info{creation_time} = '';
  $$info{publication_time} = '';
  $$info{store_time} = '';
  $$info{seen_time} = '';
  $$info{url} = '';
  $$info{total_square} = '';
  $$info{living_square} = '';
  $$info{kitchen_square} = '';
  $$info{rooms_count} = '';
  $$info{storeys} = '';
  $$info{storey} = '';
  $$info{balcony} = '';
  $$info{published} = '';
  $$info{hidden} = '';
#  $$info{deleted} = '';
#  $$info{has_photos} = '';
  $$info{bath_joint} = '';
  $$info{other_info} = '';
  $$info{debug_data} = '';
  $$info{geo_lat} = '';
  $$info{geo_lon} = '';
  $$info{house_cond} = '';

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
