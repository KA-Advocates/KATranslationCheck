$(document).ready(function() {
    $( ".hit" ).each(function(index) {
      var hittext = $(this).find(".hittext")
      $(this).find(".translated").highlight(hittext.text())
    });
    //Add report functionality
    $(".report-button").click(function(event) {
      var hit = $(this).parent();
      var filenameVal = $(hit).find(".filename").text();
      var msgstrVal = $(hit).find(".translated").html();
      var msgidVal = $(hit).find(".original").html();
      var hitVal = $(hit).find(".hittext").text();
      var urlVal = window.location.href;
      //Ask user for reason
      var reason = prompt("Please enter the reason for your report", "");
      //Submit report
      $.post("/report.php", {filename: filenameVal, msgstr: msgstrVal, msgid: msgidVal, url: urlVal, hit:hitVal, reason: reason});
    });
    //Add show image functionality
    $(".show-images-button").click(function(event) {
      var imagelist = $(this).parent();
      $(imagelist).find(".image-link").each(function(index) {
          var href = $(this).attr('href');
          $(this).replaceWith('<a href="' + href + '"><img src="' + href + '"/></a>');
      });
    });
    //Open comment links in new tab
    $(".tcomment a").attr("target", "_blank");
  })