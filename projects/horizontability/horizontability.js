var mousewheel = {
	init: function() {
		if (window.addEventListener)
        /** DOMMouseScroll is for mozilla. */
        window.addEventListener('DOMMouseScroll', mousewheel.handleWheel, false);
		/** IE/Opera. */
		//window.onmousewheel = document.onmousewheel = mousewheel.handleWheel;
		document.onmousewheel = mousewheel.handleWheel;
	},
	handleWheel: function(event) {
		var delta = 0;
		if (!event) /* For IE. */
			event = window.event;
		if (event.wheelDelta) { /* IE/Opera. */
			delta = event.wheelDelta/120;
			/** In Opera 9, delta differs in sign as compared to IE.
			*/
			if (window.opera)
				delta = -delta;
		} else if (event.detail) { /** Mozilla case. */
			/** In Mozilla, sign of delta is different than in IE.
			 * Also, delta is multiple of 3.
			*/
			delta = -event.detail/3;
		}
		/** If delta is nonzero, handle it.
		* Basically, delta is now positive if wheel was scrolled up,
		* and negative, if wheel was scrolled down.
		*/
		if (delta)
			mousewheel.handleDelta(delta);
		/** Prevent default actions caused by mouse wheel.
		* That might be ugly, but we handle scrolls somehow
		* anyway, so don't bother here..
		*/
		if (event.preventDefault)
			event.preventDefault();
		event.returnValue = false;
	},
	handleDelta: function(delta) {
		if (delta < 0)
			horizontal.scrollRight();
		else
			horizontal.scrollLeft();
	},
};

var horizontal = {
	scrollbarHeight: 0,
	margin: 40,
	step: 30,
	toolbarWidth: 54,
	columnWidth: 0,

	init: function() {
		horizontal.getScrollbarHeight();
		horizontal.resize();
		//intercepting mousewheel events causes too much havoc on mouseless devices
		//mousewheel.init();
		
		window.onkeydown = horizontal.handleKeyboard;
		
		window.onresize = horizontal.resize;
	},

	resize: function() {
		var columns = document.getElementById('readInner');
		columns.style.height = window.innerHeight - horizontal.margin - horizontal.scrollbarHeight + "px";
	},
	scrollLeft: function() {
		horizontal.scroll(-1*horizontal.step);
	},
	scrollRight: function() {
		horizontal.scroll(horizontal.step);
	},
	scroll: function(howmuch) {
		//if (document.documentElement)
			document.documentElement.scrollLeft += howmuch;
		//else 
			document.body.scrollLeft += howmuch;
	},
	pageLeft: function() {
		horizontal.getColumnWidth();
		horizontal.scroll(-horizontal.columnWidth);
	},
	pageRight: function() {
		horizontal.getColumnWidth();
		horizontal.scroll(horizontal.columnWidth);
	},
	getColumnWidth: function() {
		var container = document.getElementById('readInner');
		var width = container.firstChild.offsetWidth;
		var windowWidth = container.offsetWidth - horizontal.toolbarWidth;
		var offset = Math.ceil((windowWidth % width) / (parseInt(windowWidth / width) - 1));
		horizontal.columnWidth = container.firstChild.offsetWidth + offset;
	},
	handleKeyboard: function(e) {
		switch(e.keyCode) {
			//console.log(e.keyCode);
			//up arrow
			case 38:
				horizontal.pageLeft();
				e.preventDefault();
				break;
			//left arrow
			case 37:
				horizontal.pageLeft();
				e.preventDefault();
				break;
			//page up
			case 33:
				horizontal.pageLeft();
				e.preventDefault();
				break;
			//right arrow
			case 39:
				horizontal.pageRight();
				e.preventDefault();
				break;
			//down arrow
			case 40:
				horizontal.pageRight();
				e.preventDefault();
				break;
			//page down
			case 34:
				horizontal.pageRight();
				e.preventDefault();
				break;
			//space bar
			case 32:
				horizontal.pageRight();
				e.preventDefault();
				break;
		}
	},
	getScrollbarHeight: function() {
		var i = document.createElement('p');
		i.style.width = '100%';

		i.style.height = '200px';

		var o = document.createElement('div');
		o.style.position = 'absolute';
		o.style.top = '0px';
		o.style.left = '0px';
		o.style.visibility = 'hidden';
		o.style.width = '200px';
		o.style.height = '150px';
		o.style.overflow = 'hidden';
		o.appendChild(i);

		document.body.appendChild(o);
		//var w1 = i.offsetWidth;
		var h1 = i.offsetHeight;
		o.style.overflow = 'scroll';
		//var w2 = i.offsetWidth;
		var h2 = i.offsetHeight;
		//if (w1 == w2) w2 = o.clientWidth;
		if (h1 == h2) h2 = o.clientWidth;

		document.body.removeChild(o);

		//window.scrollbarWidth = w1-w2;
		horizontal.scrollbarHeight = h1-h2;
	}
};


