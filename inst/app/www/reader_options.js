$(document).ready(function() {
  $("header").find("nav").append('<span class="header-title"> <i>reade</i>R </span>');
  
  $(".sidebar").on("click", ".disabled", function (e) {
    e.preventDefault();
    return false;
  });
});
