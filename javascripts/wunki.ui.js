jQuery(document).ready(function($) {
    $('code').each(function(){
       var title = $(this).attr("class");
       if(title != ''){
           if(title.match('sourceCode')){
               var title = title.split(' ')[1];
           };
           $(this).before('<span class="language">'+title+'</span>');
       };
    });

    // Timeago
    $("time.timeago").timeago();
		$().UItoTop({ easingType: 'easeOutExpo' });

    // Scroll to anchors
    $("a").each(function(idx) {
      if ($(this).attr('href').match(/^#/)) {
        $(this).anchorScroll({fx: 'easeOutExpo'});
      }
    }); 
});
