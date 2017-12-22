(function() {
    if (!WebTorrent.WEBRTC_SUPPORT) return;

    var testEl = document.createElement('video');
    function testType(mime) {
        return !! testEl.canPlayType(mime);
    }

    var webTorrent = null;
    function addTorrent(torrentId, cb) {
        if (webTorrent === null) {
            webTorrent = new WebTorrent();
        }
        webTorrent.add(torrentId, {}, cb);
    }

$('.torrent').each(function() {
    var el = $(this);
    var a = el.find('a');
    var mimeType = a.attr('type');
    if (!testType(mimeType)) {
        /* No playback support */
        console.log("No playback support for:", mimeType);
        return;
    }

    var playButton = $("<a class='webtorrent-play' title='Play with WebTorrent'>⯈</a>");

    playButton.click(function() {
        playButton.remove();

        var container = $("<div class='webtorrent-container'></div>");
        container.insertBefore(el.parent())

        var torrentId = (document.location.origin || document.location.host) +
            a.attr('href') +
            "/for/webtorrent";
        addTorrent(torrentId, function(torrent) {
            torrent.files[0].appendTo(container.get(0));

            var statsEl = $("<p class='webtorrent-stats'></p>");
            statsEl.insertBefore(el.parent())

            setInterval(function() {
                var nonWebSeedWires = 0;
                for(var i = 0; i < torrent.wires.length; i++) {
                    var wire = torrent.wires[i];
                    if (wire['type'] !== 'webSeed') {
                        nonWebSeedWires += 1;
                    }
                }

                statsEl.text(
                    torrent.progress < 1 ?
                        "Downloaded " + Math.round(100 * torrent.progress) + "% from " + nonWebSeedWires + " peers at " + formatSize(torrent.downloadSpeed) + "/s" :
                        torrent.uploadSpeed > 0 ?
                        "Seeding, uploading to " + nonWebSeedWires + " peers at " + formatSize(torrent.uploadSpeed) + "/s" :
                        "Seeding"
                );
            }, 1000);
        });
    });
    el.append(playButton);
});

function formatSize(size) {
    var units = ["G", "M", "K"];
    var unit = "";
    while(size > 1024 && units.length > 0) {
        unit = units.pop();
        size /= 1024;
    }
    return (size < 8 ?
            Math.round(10 * size) / 10 :
            Math.round(size)) +
        " " + unit + "B";
}

})();
