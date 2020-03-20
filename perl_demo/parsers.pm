package parsers;

our(@ISA, @EXPORT_OK, @EXPORT);

use strict;
use warnings;
#use POSIX;
use utf8;
use Exporter ();
use Data::Dumper;

@ISA = qw(Exporter);
@EXPORT = qw(phone_for_screen phone_for_db);
@EXPORT_OK = qw();

=encoding utf8

=head1 NAME

  B<parsers.pm>

=head1 SYNOPSIS

  use parsers;

=cut

#sub timestr {
#  POSIX::strftime('%d/%m/%Y %H:%M:%S: ', localtime(time));
#}

# format phone # for screen
sub phone_for_screen {
  my $phone = shift;
  $phone =~ s/^[78](\d{3})(\d{3})(\d{4})$/+7 ($1) $2-$3/;
  $phone;
}

# format phone # for database
sub phone_for_db {
  my $phone = shift;
  $phone =~ s/^[78](\d{10})$/8$1/;
  $phone;
}

# new(%args)
sub new($$) {
  my $class = shift;
  my %self = @_;

  bless {
    dbh		=> ($self{dbh} || die "dbh (database handle) is undefined"),
    ua		=> ($self{ua} || die "ua (LWP::UserAgent) is undefined"),
    list_only	=> ($self{list_only} || 0), # если = 1, то сканировать только список и не качать страницы описаний
  }, $class;
}

# $DB::signal = $DB::signal = 1;

sub log_any {
#$DB::signal = $DB::signal = 1;
  my($self, $level, $line) = splice(@_, 0, 3);
  my($pak,$fil,$sub) = (caller(2))[0,1,3];
#    my($sub,$lin) = @$caller;
#    $caller_info = "[$pak:$fil:$lin:$sub]";
  my $caller_info = "[$sub:$line]";
#$DB::signal = $DB::signal = 1;
  Log::Any->get_logger->$level("[$level:".ref($self)."]", $caller_info, @_);
}

for(qw(trace debug info notice warning error critical alert emergency)) {
  eval qq!sub log_$_ { my \$self = shift; \$self->log_any('$_', \@_); }!;
}

# выключить некоторые сообщения
{ no warnings 'redefine';

  for(qw(trace)) {
#    eval qq!sub log_$_ { }!;
  }
}

sub log_dbh_critical {
  my $self = shift;
  $self->log_critical(@_, $$self{dbh}->errstr, $$self{dbh}->{Statement});
  die $$self{dbh}->errstr;
}

sub log_parse_error_critical {
  my($self, $line, $url, $html, $msg) = @_;
  $$self{dbh}->do("INSERT INTO parser_errors (error_msg, url, html) VALUES (?,?,?)", undef, $msg, $url, $html);
  $self->log_critical($msg);
  die $msg;
}

# проверяет наличие записи в базе
sub known {
  my($self, $info) = @_;
$self->log_trace(__LINE__, ' ');

  my($known) = $$self{dbh}->selectrow_array("SELECT count(*) FROM estates WHERE url=?", undef, $$info{url});
$self->log_trace(__LINE__, ($known ? 'UPDATE' : 'INSERT'));
  $known;
}

# $self->parse($url)
# главный цикл парсинга
sub parse {
  my($self, $url) = @_;
$self->log_trace(__LINE__, ' ');

  my $html;

  while() {
#$DB::signal = $DB::signal = 1;
    $self->log_info(__LINE__, "GET LIST $url");
#exit(0);
    my $response = $$self{ua}->get($url);
    unless($response->is_success) {
      $self->log_error(__LINE__, "GET ERROR ", $response->status_line);
      last;
    }
    $html = $response->decoded_content;
    $self->log_debug(__LINE__, "\$html UTF flag is:", utf8::is_utf8($html));
    $url = $self->parse_list($response->base, $url, $html) or last;
#$DB::signal = $DB::signal = 1;
    $url = URI->new_abs($url, $response->base)->as_string;
  }
}

# решает, что делать с информацией, собранной при парсинге страницы списка
sub dispatch {
  my($self, $info) = @_;
$self->log_trace(__LINE__, ' ');

#$DB::signal = $DB::signal = 1;
#  $self->log_trace(__LINE__, Dumper $info);
  $self->log_info(__LINE__, "PROCESSING # $$info{_found_current} OF $$info{_found_total}: $$info{url}");
#exit(0);
  my $known = $self->known($info);
  if($known && $$self{list_only}) {
    $self->update_short($info);
  } else {
    $self->parse_single($info);
    $self->cleanup($info);
    $known ? $self->update_full($info) : $self->store_new($info);
  }
}

# compare two possible undefs
sub cmp_vars {
  defined($_[0]) && defined($_[1]) ?
    $_[0]<=>$_[1] :
    !defined($_[0]) && !defined($_[1]) ?
    0 :
    !defined($_[0]) ?
    -1 :
    !defined($_[1]) ?
      1 :
      0;
}

sub update_short {
  my($self, $info) = @_;
$self->log_trace(__LINE__, ' ');
  my $dbh = $$self{dbh};

#$DB::signal = $DB::signal = 1;
  my($price_old, $estate_id, $published) = $dbh->selectrow_array("SELECT p.price,p.estate_id,e.published FROM prices AS p,estates AS e WHERE p.estate_id=e.estate_id AND e.url=? ORDER BY p.price_time DESC LIMIT 1",
    undef, $$info{url});

  $self->log_info(__LINE__, "\$estate_id = $estate_id");

  local $dbh->{AutoCommit} = 0;
  local $dbh->{RaiseError} = 1;
  eval {
    if(!$published) {
      $dbh->do("UPDATE estates SET published=1 WHERE estate_id=?", undef, $estate_id)
        or $self->log_dbh_critical(__LINE__, "UPDATE published=1 ERROR", $estate_id);
      $self->log_info(__LINE__, "СНОВА АКТИВНО");
    }

    if(cmp_vars($$info{price}, $price_old) != 0) {
      $self->log_info(__LINE__, (cmp_vars($$info{price}, $price_old) < 0 ? "ЦЕНА СНИЗИЛАСЬ С" : "ЦЕНА ПОВЫСИЛАСЬ С"), $price_old, "ДО", $$info{price});
      $dbh->do("INSERT INTO prices (estate_id,price_time,price) VALUES (?,(datetime('now','localtime')),?)", undef, $estate_id, $$info{price})
        or $self->log_dbh_critical(__LINE__, "INSERT ERROR", $estate_id, $$info{price});
    } else {
      $self->log_info(__LINE__, "ЦЕНА НЕ ИЗМЕНИЛАСЬ");
    }
    $dbh->commit;
  };
  if($@) {
    eval { $dbh->rollback };
    die $@;
  }
  1;
#  exit(0);
}

sub update_full {
  my($self, $info) = @_;
$self->log_trace(__LINE__, ' ');
  my $dbh = $$self{dbh};

#$DB::signal = $DB::signal = 1;
  my($price_old, $estate_id, $published, $full_desc) = $dbh->selectrow_array("SELECT p.price,p.estate_id,e.published,e.full_desc FROM prices AS p,estates AS e WHERE p.estate_id=e.estate_id AND e.url=? ORDER BY p.price_time DESC LIMIT 1",
    undef, $$info{url});

  $self->log_info(__LINE__, "\$estate_id = $estate_id");

  local $dbh->{AutoCommit} = 0;
  local $dbh->{RaiseError} = 1;
  eval {
    if(!$published) {
      $dbh->do("UPDATE estates SET published=1 WHERE estate_id=?", undef, $estate_id)
        or $self->log_dbh_critical(__LINE__, "UPDATE published=1 ERROR", $estate_id);
      $self->log_info(__LINE__, "СНОВА АКТИВНО");
    }

    if($full_desc ne $$info{full_desc}) {
      $dbh->do("UPDATE estates SET full_desc=? WHERE estate_id=?", undef, $$info{full_desc}, $estate_id)
        or $self->log_dbh_critical(__LINE__, "UPDATE estate.full_desc ERROR", $estate_id, $$info{full_desc});
      $self->log_info(__LINE__, "ОПИСАНИЕ ИЗМЕНИЛОСЬ: $$info{full_desc}");
    }

    if(cmp_vars($$info{price}, $price_old) != 0) {
      $self->log_info(__LINE__, (cmp_vars($$info{price}, $price_old) < 0 ? "ЦЕНА СНИЗИЛАСЬ С" : "ЦЕНА ПОВЫСИЛАСЬ С"), $price_old, "ДО", $$info{price});
      $dbh->do("INSERT INTO prices (estate_id,price_time,price) VALUES (?,(datetime('now','localtime')),?)", undef, $estate_id, $$info{price})
        or $self->log_dbh_critical(__LINE__, "INSERT ERROR", $estate_id, $$info{price});
    } else {
      $self->log_info(__LINE__, "ЦЕНА НЕ ИЗМЕНИЛАСЬ");
    }
    $dbh->commit;
  };
  if($@) {
    eval { $dbh->rollback };
    die $@;
  }
  1;
#  exit(0);
}

sub select_or_insert {
  my($self, $select_sql, $select_params, $insert_sql, $insert_params) = @_;
  $self->log_trace(__LINE__, ' ');
  my $dbh = $$self{dbh};

  my $ret = $dbh->selectrow_arrayref($select_sql, undef, @$select_params);
  unless($ret) {
    eval {
      local $dbh->{RaiseError} = 1;
      $dbh->do($insert_sql, undef, @$insert_params);
      $ret = $dbh->selectrow_arrayref($select_sql, undef, @$select_params);
    };
    $self->log_dbh_critical(__LINE__, "WRONG QUERY", @$insert_params) if $@;
  }

  @$ret;
}

sub region_id {
  my($self, $region) = @_;
  $self->log_trace(__LINE__, ' ');

  ($self->select_or_insert(
    "SELECT region_id FROM regions WHERE region_name=?" => [$region],
    "INSERT INTO regions (region_name) VALUES (?)" => [$region],
  ))[0];
}

sub city_id {
  my($self, $region_id, $city) = @_;
  $self->log_trace(__LINE__, ' ');

  ($self->select_or_insert(
    "SELECT city_id FROM cities WHERE region_id=? AND city_name=?" => [$region_id, $city],
    "INSERT INTO cities (region_id, city_name) VALUES (?,?)" => [$region_id, $city],
  ))[0];
}

sub district_id {
  my($self, $city_id, $district) = @_;
  $self->log_trace(__LINE__, ' ');

  ($self->select_or_insert(
    "SELECT district_id FROM districts WHERE city_id=? AND district_name=?" => [$city_id, $district],
    "INSERT INTO districts (city_id, district_name) VALUES (?,?)" => [$city_id, $district],
  ))[0];
}

sub store_new {
#$DB::signal = $DB::signal = 1;
  my($self, $info) = @_;
  $self->log_trace(__LINE__, ' ');
  my $dbh = $$self{dbh};

  $self->log_info(__LINE__, "НОВАЯ ЗАПИСЬ");
  local $dbh->{AutoCommit} = 0;
  local $dbh->{RaiseError} = 1;
  eval {
#    $dbh->begin_work;
    ($$info{site_id}) = $dbh->selectrow_array("SELECT site_id FROM sites WHERE site_class=?", undef, ref($self));
    $dbh->do("INSERT INTO estates (region_id,site_id,city_id,district_id,person,street,street_no,site_pub_id,title,full_desc,creation_time,publication_time,url,total_square,living_square,kitchen_square,rooms_count,storeys,storey,balcony,bath_joint,other_info,house_cond,debug_data,geo_lat,geo_lon) "
    ."VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)",
      undef,
      @$info{qw(region_id site_id city_id district_id person street street_no site_pub_id title full_desc creation_time publication_time url total_square living_square kitchen_square rooms_count storeys storey balcony bath_joint other_info house_cond debug_data geo_lat geo_lon)}
    );
#  $self->log_info(__LINE__, "estates added");
    ($$info{estate_id}) = $dbh->selectrow_array("SELECT estate_id FROM estates WHERE url=?", undef, $$info{url});
  $self->log_info(__LINE__, "estate_id is $$info{estate_id}");
    $dbh->do("INSERT INTO seen_count (estate_id,seen_time,seen_count) VALUES (?,(datetime('now','localtime')),?)", undef, @$info{qw(estate_id seen_count)})
      if $$info{seen_count};
  $self->log_info(__LINE__, "seen_count added");
    $dbh->do("INSERT INTO prices (estate_id,price_time,price) VALUES (?,(datetime('now','localtime')),?)", undef, @$info{qw(estate_id price)});
  $self->log_info(__LINE__, "prices added");
    for(@{$$info{phones}}) {
      $dbh->do("INSERT INTO phones(estate_id,phone) VALUES (?,?)", undef, $$info{estate_id}, $_)
        if defined($_) && /\d+/;
    }
    $dbh->commit;
  };
  if($@) {
    eval { $dbh->rollback };
    die $@;
  }
#  $dbh->trace("0");
#  my($district) = $dbh->selectrow_array("SELECT district_name_dst FROM district_tr WHERE site_id=? AND city_id=? AND district_name_src=?",
#    undef, ref($self), $$info{city_id}, $$info{district},
#  ) or $self->log_critical("DATABASE FETCH ERROR", $dbh->errstr), die $dbh->errstr;
#exit(0);
  1;
}

1;