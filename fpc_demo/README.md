### My Delphi/FreePascal programming skills demo

Namely, [Free Pascal](https://www.freepascal.org/) and [Lazarus IDE](https://www.lazarus-ide.org/) in this case

The desktop part of the real estate classification software package

The [scripts](../perl_demo) written on [Perl](https://www.perl.org/) collect the data to SQL DB ([SQLite](https://www.sqlite.org/) in this case) and the operator analyze them

[Chromium Embedded Framework](https://github.com/chromiumembedded) (it's FPC library [fpCEF3](https://github.com/dliw/fpCEF3) to be exact) is used for the data presentation

For the user convenience the simple ads blocking is realized based on hosts blacklist (see [adaway_hosts](./adaway_hosts))
***
[\_build.bat](\_build.bat) - builds project  
[\_makeflt.cmd](\_makeflt.cmd) - recreates ads filter  
[\_pasdoc.cmd](\_pasdoc.cmd) - recreates documentation  
[chromium_ext.pas](chromium_ext.pas) - extended TChromium with the ads blocking and other special features  
[estate.sql](estate.sql) - database structure  
[estate_informer.lpr](estate_informer.lpr) - main project file  
[informer_filter.src.txt](informer_filter.src.txt) - user hosts filter  
[main.lfm](main.lfm) - main form GUI  
[main.pas](main.pas) - main form logic  
[parsers.pas](parsers.pas), [parser_utils.pas](parser_utils.pas) - auxiliary logic  
[warnings.inc](warnings.inc) - compilation hints tuning
***

Пример навыков программирования на Delphi/FreePascal

В данном случае на FreePascal в среде Lazarus

Для отображения данных используется Chromium Embedded Framework в варианте fpCEF3 с блокировщиком рекламы на основе чёрного списка хостов

Пользовательский интерактивный модуль программного комплекса для обработки объявлений недвижимости подключается к базе SQL (в данном конкретном случае - SQLite), куда собираются данные извне (в данном случае - при помощи Perl-скриптов)