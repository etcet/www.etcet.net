jQuery(document).ready(function($) {
    $('pre').each(function(){
       var title = $(this).attr("class");
       if(title != ''){
           if(title.match('sourceCode')){
               var title = title.split(' ')[1];
           };
           $(this).prepend('<span class="language">'+title+'</span>');
       };
    });

    // Timeago
    $("time.timeago").timeago();
		$().UItoTop({ easingType: 'linear' });
});
