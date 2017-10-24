(function() {
    var proto = document.location.protocol == "https:" ?
        "wss" : "ws";
    var torrentOpts = {
        announce: [proto + "://" + document.location.host + "/webtorrent-announce"]
    };
    
    var webTorrent = null;
    function addTorrent(torrentId, cb) {
        if (webTorrent === null) {
            webTorrent = new WebTorrent();
        }
        webTorrent.add(torrentId, torrentOpts, cb);
    }
    
$('.torrent').each(function() {
    var el = $(this);
    var playButton = $("<span class='webtorrent-play'>⯈</span>");

    playButton.click(function() {
        playButton.remove();

        var container = $('<div></div>');
        container.insertBefore(el.parent())

        var torrentId = (document.location.origin || document.location.host) +
            el.find('a').attr('href');
        addTorrent(torrentId, function(torrent) {
            torrent.files[0].appendTo(container.get(0));
        });
    });
    el.append(playButton);
});

})();
