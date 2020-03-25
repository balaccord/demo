## My programming skills demos

##### All these things are used in real life

### Python and shell scripting
[websocket_asyncio_demo.py](websocket_asyncio_demo.py) - Operator browser workplace data logger, slightly adopted (the site name is changed). [The Google Chrome Dev Protocol](https://chromedevtools.github.io/devtools-protocol/tot) is used for the data handling  
[log_compress.sh](log_compress.sh) - complicated compression utility of the logs above, format is FLZMA2 7-zip: the tightest compress ratio with the very fast unpacking
[hdf5_gz2zstd.py](hdf5_gz2zstd.py) - HDF5 table data GZ -> ZSTD recompression (inspired by these tests: [compression_results.md](compression_results.md))  
[gmail-send.py](gmail-send.py) - GMail files sender. My own life became so easy with it :)  
***
### [Delphi/FreePascal](fpc_demo)

***
### [Perl](perl_demo)

Сборщик данных реального времени, поступающих через websocket на браузер рабочего места оператора   
Замысловатое сжатие этих данных  
Перепаковка из GZ в ZSTD данных, хранящихся в таблице HDF5. Идея была навеяна тестами сжатия ZSTD со словарём  
Максимально упрощённая отправка файлов по гугловской почте  
