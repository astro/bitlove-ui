{ mkDerivation, aeson, attoparsec, base, binary, blaze-builder
, blaze-html, blaze-markup, bytestring, case-insensitive, cereal
, clientsession, conduit, containers, convertible, cookie
, crypto-api, cryptohash, cryptohash-cryptoapi, data-default
, directory, enclosed-exceptions, file-embed, hamlet, hashable
, hjsmin, http-conduit, http-types, lifted-base, memcache
, mime-mail, monad-control, mtl, network, network-uri, old-locale
, postgresql-binary, postgresql-libpq, random, regex-pcre
, resource-pool, resourcet, shakespeare, shakespeare-css
, shakespeare-js, shakespeare-text, stdenv, stm, systemd
, template-haskell, text, time, unordered-containers, wai
, wai-extra, warp, warp-tls, xml-conduit, xml-types, yaml, yesod
, yesod-auth, yesod-core, yesod-form, yesod-static
, yesod-websockets
}:
mkDerivation {
  pname = "bitlove-ui";
  version = "0.0.1";
  src = /home/stephan/bitlove-ui;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson attoparsec base binary blaze-builder blaze-html blaze-markup
    bytestring case-insensitive cereal clientsession conduit containers
    convertible cookie crypto-api cryptohash cryptohash-cryptoapi
    data-default directory enclosed-exceptions file-embed hamlet
    hashable hjsmin http-conduit http-types lifted-base memcache
    mime-mail monad-control mtl network network-uri old-locale
    postgresql-binary postgresql-libpq random regex-pcre resource-pool
    resourcet shakespeare shakespeare-css shakespeare-js
    shakespeare-text stm systemd template-haskell text time
    unordered-containers wai wai-extra xml-conduit xml-types yaml yesod
    yesod-auth yesod-core yesod-form yesod-static yesod-websockets
  ];
  executableHaskellDepends = [ base warp warp-tls yesod yesod-core ];
  doHaddock = false;
  homepage = "https://bitlove.org/";
  description = "The greatest Yesod web application ever";
  license = stdenv.lib.licenses.bsd3;
}
