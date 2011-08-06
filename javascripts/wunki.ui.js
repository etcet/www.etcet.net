jQuery(document).ready(function($) {
    // Show the language used in the <pre>
    /*
       $('pre').each(function(){
       var title = $(this).attr("class");
       if(title != ''){
       if(title.match('sourceCode')){
       var title = title.split(' ')[1];
       };
       $(this).prepend('<span class="language">'+title+'</span>');
       };
       });
       */

    // Timeago
    $("time.timeago").timeago();

    // Define the height of the bottom and middle papers
    var page_height = $("#top-page").height();
    if (page_height > 989){ page_height = 989; }
    $("#bottom-page").css("height", page_height);
    $("#middle-page").css("height", page_height);
    
    /* Scrolling header. Disable for now.
    function place_scrolly_header(){
        var c = 75;
        var e = $(window).scrollTop();
        var d = null;
        var a = null;
        var b = null;
        
        h2s.each(function(){
            var h = $(this).position()["top"]-c;
            if(e < h){ return false }
            b = h;
            d = $(this).html().replace(/&nbsp;/g," ");
            var f = e - (b+c);
            
            a = f / c;
            if ( a > 1){ a = 1}
            if (a > 0.99){var j=$(this).nextAll("h2");
                if(j.length){
                    var i=j.first().position()["top"];
                    var g=i-e;
                    
                    if(g<=c*2){
                        a=1/(c-g/2)
                    }
                }
            }
        });
        $("#scrolling-header").css({opacity:a}).css("left",h2s.first().position()["left"]-220-35).html(d)
    };
    
    $(function(){
        if($(".meta").length){
            $("body").append('<div id="scrolling-header"></div>');
            h2s=$("article h2");
            $(window).scroll(function(){
                place_scrolly_header()
            })}
    });
    */
});
