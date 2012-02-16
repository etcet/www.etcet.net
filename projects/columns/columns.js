var width;	//width of a single column

$(window).load(function() {
	//set column height, introduce bottom scroll bar
	resize_columns();
	//set column height again, compensating for bottom scroll bar
	resize_columns();
	
	//keydown callback, arrow keys move one column left/right
	$(window).keydown(function (e) {
		switch (e.which) {
			case 38:
			case 37 : $(window).scrollLeft($(window).scrollLeft() - width); e.preventDefault(); break;
			case 40:
			case 39: $(window).scrollLeft($(window).scrollLeft() + width); e.preventDefault(); break;
		}
	});
	
	//mousewheel callback, each click is one column left/right
	$(window).mousewheel(function(e, delta) {
		$(window).scrollLeft($(window).scrollLeft() - delta * width);
		e.preventDefault();
	});
	
	//on window resize, change column height
	$(window).resize(resize_columns);
});

//resize column height and calculate new column width
function resize_columns() {
	var offset = $('.columns').offset();
	
	//set column height to window 'outer' height
	var height = $(window).height() - 2 * offset.top;
	$('.columns').height(height);
	
	if ($('.columns').css('-moz-column-width')) {
		var column_width = parseInt($('.columns').css('-moz-column-width').replace(/px,*\)*/g,"")) + 20; //offset by moz strangeness
		var view_width = $(window).width() - 32; //offset by padding
		if (view_width < column_width) { column_width = view_width; } //check for dbz
		width = Math.floor(view_width / Math.floor(view_width / column_width)) - 2;
		
	}
	else if ($('.columns').css('-webkit-column-width')) {
		var column_width = parseInt($('.columns').css('-webkit-column-width').replace(/px,*\)*/g,"")) + 16;
		var view_width = $(window).width() - 38;
		if (view_width < column_width) { column_width = view_width; }
		width = Math.floor(view_width / Math.floor(view_width / column_width)) - 2;
	}
}
