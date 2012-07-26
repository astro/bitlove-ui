SET default_tablespace = fast;

CREATE TABLE cached_images (
       "url" TEXT NOT NULL,
       "size" INT NOT NULL,
       "time" TIMESTAMP,
       "error" TEXT,
       "data" BYTEA,
       PRIMARY KEY ("url", "size")
);
