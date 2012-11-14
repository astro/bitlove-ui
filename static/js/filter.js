(function() {

function mapType(type) {
    switch(type) {
	case 'application/ogg':
	    return 'audio';
	case 'application/pdf':
	    return 'text';
	default:
	    return type.split('/')[0];
    }
}

function Filterable(el) {
    this.el = el;
}
Filterable.prototype = {
    applyMask: function(mask) {
	var langMatch = this.langs.some(function(lang) {
	    return !mask.lang[lang];
	});
	var typeMatch = this.types.some(function(type) {
	    return !mask.type[type];
	});

	if (typeMatch && langMatch)
	    this.show();
	else
	    this.hide();
    },
    show: function() {
	this.el.removeClass('filteredout');
	this.el.addClass('filteredin');
    },
    hide: function() {
	this.el.addClass('filteredout');
	this.el.removeClass('filteredin');
    },
    isVisible: function() {
	return this.el.hasClass('filteredin');
    }
};

function FilterableItem(el) {
    Filterable.call(this, el);

    this.langs = [el.attr('xml:lang')];
    var types = this.types = [];
    el.find('.torrent a').each(function() {
	types.push(mapType($(this).data('type')));
    });
}
FilterableItem.prototype = new Filterable();

function FilterableFeed(el) {
    Filterable.call(this, el);

    this.el = el;
    var lang = el.attr('xml:lang');
    this.langs = lang ? [lang] : [];
    this.types = ("" + el.data('types')).split(',').map(mapType);
}
FilterableFeed.prototype = new Filterable();

function FilterableUser(el) {
    Filterable.call(this, el);

    var langs = this.langs = [];
    var types = this.types = [];
    this.feeds = el.find('ul.feeds > li').map(function() {
	var feed = new FilterableFeed($(this));
	langs.push.apply(langs, feed.langs);
	types.push.apply(types, feed.types);
	return feed;
    }).toArray();
}
FilterableUser.prototype = new Filterable();
FilterableUser.prototype.applyMask = function(mask) {
    var anyVisible = false;
    this.feeds.forEach(function(feed) {
	feed.applyMask(mask);
	anyVisible = anyVisible || feed.isVisible();
    });
    if (anyVisible)
	this.show();
    else {
	this.hide();
	/* Avoid double opacity drop */
	this.feeds.forEach(function(feed) {
	    feed.show();
	});
    }
};

var filterItems = [];
$('.item').each(function() {
    var el = $(this);
    filterItems.push(new FilterableItem($(this)));
});
var Filter = {
    items: $('.item').map(function() {
	    return new FilterableItem($(this));
        }).toArray().concat($('.directory .meta').map(function() {
	    return new FilterableUser($(this));
	}).toArray()),

    isAllowedType: function(type) {
	return ["text", "application", "audio", "video", "message", "image"
	       ].indexOf(type) >= 0;
    },

    getAllTypes: function() {
	var types = {}, i, j;
	for(i = 0; i < Filter.items.length; i++)
	    for(j = 0; j < Filter.items[i].types.length; j++) {
		var type = Filter.items[i].types[j];
		if (!Filter.isAllowedType(type))
		    continue;
		if (!types.hasOwnProperty(type))
		    types[type] = 0;
		types[type]++;
	    }
	return types;
    },
    getAllLangs: function() {
	var langs = {}, i, j;
	for(i = 0; i < Filter.items.length; i++)
	    for(j = 0; j < Filter.items[i].langs.length; j++) {
		var lang = Filter.items[i].langs[j];
		if (!langs.hasOwnProperty(lang))
		    langs[lang] = 0;
		langs[lang]++;
	    }
	return langs;
    },

    /* Inverted: */
    mask: {
	type: {},
	lang: {}
    },

    applyMask: function() {
	for(var i = 0; i < Filter.items.length; i++)
	    Filter.items[i].applyMask(Filter.mask);

	if (window.localStorage && window.localStorage.setItem)
	    window.localStorage.setItem('Prittorrent.UI.Filter.Mask', JSON.stringify(Filter.mask));
    }
};

try {
    if (window.localStorage && window.localStorage.getItem) {
	Filter.mask = JSON.parse(window.localStorage.getItem('Prittorrent.UI.Filter.Mask'));
	/* Repair: */
	if (!Filter.mask)
	    Filter.mask = {};
	if (!Filter.mask.hasOwnProperty('type'))
	    Filter.mask.type = {};
	if (!Filter.mask.hasOwnProperty('lang'))
	    Filter.mask.lang = {};

	Filter.applyMask();
    }

} catch (x) {
    if (window.console && window.console.error)
	window.console.error(x.stack || x);
}

function sortKeysNullLast(o) {
    return Object.keys(o).sort(function(a, b) {
	if (!a && b)
	    return 1;
	else if (a && !b)
	    return -1;
	else if (a < b)
	    return -1;
	else if (a > b)
	    return 1;
	else
	    return 0;
    });
}

function FilterDialog() {
    var that = this;
    this.el = $('<form class="filterdialog"><div class="type"><h2>By type</h2><ul></ul><p><a class="all">Select all</a></p></div><div class="lang"><h2>By language</h2><ul></ul><p><a class="all">Select all</a></p></div></form>');

    /* Add checkboxes  */
    var allTypes = Filter.getAllTypes();
    sortKeysNullLast(allTypes).forEach(function(type) {
	var text = type ?
	    (type.substr(0, 1).toLocaleUpperCase() + type.substr(1)) :
	    "Other";
	that.addOption('type', type, text, allTypes[type]);
    });
    var allLangs = Filter.getAllLangs();
    sortKeysNullLast(allLangs).forEach(function(lang) {
	var text = lang ?
	    (lang.substr(0, 1).toLocaleUpperCase() + lang.substr(1)) :
	    "Other";
	that.addOption('lang', lang, text, allLangs[lang]);
    });

    /* Select all */
    this.el.find('.type, .lang').each(function() {
	var col = $(this);
	col.find('.all').click(function(ev) {
	    ev.preventDefault();

	    col.find('input[type=checkbox]').prop('checked', true);
	    that.update();
	});
    });
}
FilterDialog.prototype = {
    hide: function() {
	this.el.remove();
    },
    addOption: function(column, val, text, count) {
	var li = $('<li></li>');

	var option = $('<input type="checkbox">');
	var id = "ft-" + column + "-" + val;
	option.attr('id', id);
	option.val(val);
	option.prop('checked', !Filter.mask[column][val]);
	var that = this;
	option.change(function() {
	    that.update();
	});
	li.append(option);

	var label = $('<label></label>');
	label.attr('for', id);
	label.text(text || val);
	label.append(' <span class="counter"></span>');
	label.find('.counter').text("(" + count + ")");
	li.append(label);

	this.el.find('.' + column).find('ul').append(li);
    },
    update: function() {
	var el = this.el;
	['type', 'lang'].forEach(function(column) {
	    /* Rebuild mask */
	    el.find('.' + column + ' input[type=checkbox]').each(function() {
		var input = $(this);
		Filter.mask[column][input.val()] = !input.prop('checked');
	    });
	});

	Filter.applyMask();
    }
};

var container = $('<div class="filter"></div>');
$('.feedslist').after(container);
var filterButton = $('<a class="filterbutton">Filter</a>');
container.append(filterButton);

var filterDialog;
filterButton.click(function(ev) {
    ev.preventDefault();

    if (!filterDialog) {
	filterDialog = new FilterDialog();
	container.append(filterDialog.el);
	filterButton.addClass('toggled');
    } else {
	filterDialog.hide();
	filterDialog = null;
	filterButton.removeClass('toggled');
    }
});

})();
