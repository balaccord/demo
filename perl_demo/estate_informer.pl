use strict;
use warnings;
use DBI qw(:sql_types);
use LWP::UserAgent ();
use LWP::ConnCache;
use HTTP::Cookies;
use HTML::TokeParser;
use Data::Dumper;
use utf8;
# require HTTP::Headers;
#use parsers qw(Log);
use Log::Any qw($log);
#use Log::Any::Adapter qw(Stderr);
use Log::Any::Adapter ('File', 'estate_informer.log');
use Log::Any::For::Std;

require domino;
require avito;
require domofond;
require vdv;

#use open ':utf8';
use open qw/:std :encoding(utf8)/;

binmode STDIN, ':utf8';
binmode STDERR, ':utf8';
binmode STDOUT, ':utf8';

$Data::Dumper::Sortkeys = 1;

# hack for normal unicode output http://www.perlmonks.org/?node_id=759457
$Data::Dumper::Useqq = 1;

{ no warnings 'redefine';
    sub Data::Dumper::qquote {
        my $s = shift;
        return "'$s'";
    }
}

my $ua = LWP::UserAgent->new(
  agent			=> 'Mozilla/5.0 (Windows NT 6.1; WOW64; rv:45.9) Gecko/20100101 Firefox/45.9',
  timeout		=> 30,
  cookie_jar		=> HTTP::Cookies->new(
    file		=> "estate_cookies.dat",
    autosave 		=> 1,
  ),
  conn_cache		=> LWP::ConnCache->new(
    total_capacity	=> 1,
  ),
  default_headers	=> HTTP::Headers->new(
    'Accept-Language'	=> "ru, en",
    'Accept-Charset'	=> 'utf-8',
    'Accept-Encoding'	=> 'gzip, deflate',
  ),
);

my $db = DBI->connect("dbi:SQLite:dbname=estate.sqlite", undef, undef, {
  sqlite_unicode => 1,
#  sqlite_open_flags => SQLITE_OPEN_READWRITE,
});

$db->do("PRAGMA foreign_keys = ON");
$db->do("PRAGMA journal_mode = WAL");

#open(my $log, ">>:encoding(UTF-8)", "estate_informer.log")
#  || die "Can't open UTF-8 encoded estate_informer.log: $!";

$log->info(' ');
$log->info(' ');
$log->info("################################################");
$log->info("                    START");
$log->info("################################################");
$log->info(' ');
$log->info(' ');

#exit(0);

my %parser_args = (
  ua		=> $ua,
  dbh		=> $db,
  list_only	=> 1,	# сканировать только список, не качать страницы описаний
);

site1_module->new(%parser_args)->parse('https://www.site1.ru/?para1=val1&para2=val2&...');
site2_module->new(%parser_args)->parse('https://www.site2.ru/?x1=y1&x2=y2&...');
...
siteN_module->new(%parser_args)->parse('https://www.siteN.ru/?data...');

$log->info(' ');
$log->info(' ');
$log->info("################################################");
$log->info("                    DONE");
$log->info("################################################");

1;
