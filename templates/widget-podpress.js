window.bitlovePodpress = false;

var $ = jQuery;
$(document).ready(function() {
    if (window.bitlovePodpress)
	return;
    window.bitlovePodpress = true;

    /* For Podpress */
    $('.podPress_downloadlinks').each(function() {
	var orig = $(this);
	var url = orig.find('a').attr('href');

	if (/.torrent$/.test(url))
	    return;

	resolve(url, function(info) {
	    if (window.console && console.log)
		console.log("Bitlove torrent",url,info);
	    var torrent = info && info.sources && info.sources[0] && info.sources[0].torrent;
	    if (info && torrent) {
		var t = $('<div class="podPress_downloadlinks"><a class="podpress_downloadimglink" title="Download via BitTorrent" type="application/x-bittorrent"><img src="https://bitlove.org/static/bitlove-button.png" class="podPress_imgicon"></a><span class="podpress_mediafile_title"> <span class="s"></span><span class="l"></span><span class="d"></span></span></div>');
		t.find('a').attr('href', torrent);
		t.find('.s').text(info.seeders + (info.seeders == 1 ? " Seeder, " : " Seeders, "));
		t.find('.l').text(info.leechers + (info.leechers == 1 ? " Leecher" : " Leechers"));
		if (info.downloaded == 1) {
		    t.find('.d').text(", 1 Download");
		} else if (info.downloaded > 1) {
		    t.find('.d').text(", " + info.downloaded + " Downloads");
		}

		var link = info.sources[0].permalink;
		if (link) {
		    var source = $('<span class="podpress_mediafile_dursize">on <a>Bitlove</a></span>');
		    source.find('a').attr('href', link);
		    t.append(source);
		}

		orig.after(t);
	    }
	});
    });
});
