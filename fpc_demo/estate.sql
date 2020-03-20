-- DROP TABLE sites;
CREATE TABLE sites (
  site_id INTEGER PRIMARY KEY NOT NULL,
  site_class TEXT NOT NULL,		-- название класса обработчика
  site_desc TEXT NOT NULL,		-- описание
  homepage TEXT NOT NULL		-- домашняя страница
);
CREATE UNIQUE INDEX sites_idx ON sites (site_class);


-- DROP TABLE regions;
CREATE TABLE regions (
  region_id INTEGER PRIMARY KEY NOT NULL,
  region_name TEXT NOT NULL
);

-- DROP TABLE cities;
CREATE TABLE cities (
  city_id INTEGER PRIMARY KEY NOT NULL,
  region_id INTEGER NOT NULL REFERENCES regions(region_id),
  city_name TEXT NOT NULL
);

-- DROP TABLE districts;
CREATE TABLE districts (
  district_id INTEGER PRIMARY KEY NOT NULL,
  city_id INTEGER NOT NULL REFERENCES cities(city_id),
  district_name TEXT NOT NULL
);
CREATE INDEX districts_idx ON districts (city_id, district_name);

/*
-- DROP TABLE district_tr;
CREATE TABLE district_tr (
  site_id INTEGER NOT NULL,
  city_id INTEGER NOT NULL,
  district_name_src TEXT NOT NULL,
  district_name_dst TEXT NOT NULL,
  FOREIGN KEY(site_id) REFERENCES sites(site_id),
  FOREIGN KEY(city_id) REFERENCES cities(city_id)
);
INSERT INTO district_tr VALUES (1, 1, 'Краснооктябрьский р-н', 'Краснооктябрьский');
*/

/*
-- DROP TABLE streets;
CREATE TABLE streets (
  streets_id INTEGER PRIMARY KEY NOT NULL,
  city_id INTEGER NOT NULL,
  street_name TEXT NOT NULL,
  FOREIGN KEY(city_id) REFERENCES cities(city_id)
);
CREATE INDEX streets_idx ON streets (city_id, street_name);
*/

-- DROP TABLE house_types;
/*
CREATE TABLE house_types (
  house_type_id INTEGER PRIMARY KEY NOT NULL,
  house_descr TEXT NOT NULL
);
INSERT INTO house_types (house_descr) VALUES ('Панельный');
INSERT INTO house_types (house_descr) VALUES ('Кирпичный');

-- DROP TABLE flat_types;
CREATE TABLE flat_types (
  flat_type_id INTEGER PRIMARY KEY NOT NULL,
  flat_descr TEXT NOT NULL
);
INSERT INTO flat_types (flat_descr) VALUES ('Сталинка');
INSERT INTO flat_types (flat_descr) VALUES ('Хрущёвка');
INSERT INTO flat_types (flat_descr) VALUES ('Брежневка');
INSERT INTO flat_types (flat_descr) VALUES ('Улучшенка');
INSERT INTO flat_types (flat_descr) VALUES ('Студия');
*/

-- DROP TABLE estates;
CREATE TABLE estates (
  estate_id INTEGER PRIMARY KEY NOT NULL,					-- первичный ключ
  region_id INTEGER NOT NULL REFERENCES regions(region_id),			-- район области, данные из таблицы REGIONS
  site_id INTEGER NOT NULL REFERENCES sites(site_id),			-- исходный сайт, данные из таблицы SITES
  city_id INTEGER NOT NULL REFERENCES cities(city_id),			-- населённый пункт, данные из таблицы CITIES
  district_id INTEGER NOT NULL REFERENCES districts(district_id),		-- район города, данные из таблицы DISTRICTS
--  house_type_id INTEGER NOT NULL REFERENCES house_types(house_type_id),	-- тип здания, данные из таблицы HOUSE_TYPES
--  flat_type_id INTEGER NOT NULL REFERENCES flat_types(flat_type_id),	-- тип квартиры, данные из таблицы FLAT_TYPES
  person TEXT,				-- имя контактного лица
  street TEXT COLLATE NOCASE,		-- улица
  street_no TEXT,			-- номер дома
  site_pub_id TEXT,			-- идентификатор на сайте, если есть
  title TEXT,				-- заголовок
  full_desc TEXT,			-- полное описание
  creation_time DATETIME,		-- время создания объявления
  publication_time DATETIME,		-- время последней публикации
  store_time DATETIME DEFAULT (DATETIME('now','localtime')),	-- время сохранения в базу
  seen_time DATETIME,			-- время просмотра пользователем, нужно для определения новых объявлений
  url TEXT NOT NULL,			-- исходный урл
  total_square NUMERIC,			-- общая площадь
  living_square NUMERIC,		-- жилая площадь
  kitchen_square NUMERIC,		-- площадь кухни
  rooms_count TINYINT,			-- кол-во комнат
  storeys SMALLINT,			-- этажность дома
  storey SMALLINT,			-- этаж
  balcony BOOLEAN,			-- с балконом
  published BOOLEAN NOT NULL DEFAULT 1,	-- опубликовано или нет
  hidden BOOLEAN NOT NULL DEFAULT 0,	-- прятать неинтересное
  rating SMALLINT NOT NULL DEFAULT 0,	-- рейтинг
  bath_joint BOOLEAN,			-- санузел совмещённый
  other_info TEXT,			-- доп. информация с сайта
  house_cond TEXT,			-- техническое состояние дома
  comment TEXT,				-- заметки пользователя
  debug_data TEXT,			-- отладочная информация
  geo_lat NUMERIC,
  geo_lon NUMERIC
);
CREATE INDEX estates_idx_street ON estates (street);
CREATE UNIQUE INDEX estates_idx_url ON estates (url);

-- сохранённые снимки
CREATE TABLE photos (
  estate_id INTEGER NOT NULL REFERENCES estates(estate_id),
  content_type TEXT NOT NULL,
  photo BLOB NOT NULL
);

-- кол-во просмотров на сайте
CREATE TABLE seen_count (
  estate_id INTEGER NOT NULL REFERENCES estates(estate_id),
  seen_time DATETIME NOT NULL DEFAULT (datetime('now','localtime')),
  seen_count NUMERIC NOT NULL
);
CREATE UNIQUE INDEX seen_idx ON seen_count (estate_id, seen_time);

-- динамика цен
CREATE TABLE prices (
  estate_id INTEGER NOT NULL REFERENCES estates(estate_id),
  price_time DATETIME NOT NULL DEFAULT (datetime('now','localtime')),
  price NUMERIC
);
CREATE UNIQUE INDEX price_idx ON prices (estate_id, price_time DESC);

/*DROP TABLE persons;		-- контактные лица
CREATE TABLE persons (
  person_id INTEGER PRIMARY KEY NOT NULL,
  estate_id INTEGER NOT NULL,
  name TEXT NOT NULL
);
CREATE UNIQUE INDEX person_idx ON persons (person_id, estate_id);
CREATE INDEX person_idx_name ON persons (name);*/

-- DROP TABLE phones;
CREATE TABLE phones (
  estate_id INTEGER NOT NULL REFERENCES estates(estate_id),
  phone TEXT NOT NULL
);
CREATE UNIQUE INDEX phone_idx ON phones (estate_id, phone);
CREATE INDEX phone_idx_phone ON phones (phone);

CREATE TABLE parser_errors (
  error_time INTEGER NOT NULL DEFAULT (datetime('now','localtime')),
  error_msg TEXT NOT NULL DEFAULT '',
  url TEXT NOT NULL DEFAULT '',
  html TEXT NOT NULL DEFAULT ''
);

CREATE VIEW price_view AS
SELECT
  p.estate_id AS estate_id,
  max(p.price_time) AS price_time,
  p.price AS price,
  r.region_name AS region_name,
  c.city_name AS city_name,
  d.district_name AS district_name,
  s.site_class AS site_class,
  s.homepage AS homepage,
  s.site_desc AS site_desc,
  
  e.person AS person,
  e.street AS street,
  e.street_no AS street_no,
  e.site_pub_id AS site_pub_id,
  e.title AS title,
  e.full_desc AS full_desc,
  e.creation_time AS creation_time,
  e.publication_time AS publication_time,
  e.store_time AS store_time,
  e.seen_time AS seen_time,
  e.url AS url,
  e.total_square AS total_square,
  e.living_square AS living_square,
  e.kitchen_square AS kitchen_square,
  e.rooms_count AS rooms_count,
  e.storeys AS storeys,
  e.storey AS storey,
  e.balcony AS balcony,
  e.published AS published,
  e.hidden AS hidden,
  e.rating AS rating,
  e.bath_joint AS bath_joint,
  e.other_info AS other_info,
  e.house_cond AS house_cond,
  e.comment AS comment,
  e.debug_data AS debug_data,
  e.geo_lat AS geo_lat,
  e.geo_lon AS geo_lon
FROM
  prices p
  INNER JOIN estates AS e ON p.estate_id = e.estate_id
  INNER JOIN regions AS r ON r.region_id = e.region_id
  INNER JOIN cities AS c ON c.region_id = e.region_id AND c.city_id = e.city_id
  INNER JOIN districts AS d ON d.district_id = e.district_id
  INNER JOIN sites AS s ON s.site_id = e.site_id
  GROUP BY p.estate_id
;
