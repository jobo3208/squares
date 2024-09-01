const showFeedback = function(show, delay = 0) {
  const feedback = document.getElementsByClassName("feedback");
  Array.from(feedback).forEach(function (f) {
    f.style.transition = "opacity " + delay + "s";
    f.style.opacity = show ? 1 : 0;
  });
};

document.addEventListener("DOMContentLoaded", function() {
  showFeedback(false, 0);
});

document.addEventListener("htmx:load", function() {
  showFeedback(true, 0);
  setTimeout(function() {
    showFeedback(false, 0.25);
  }, 500);
});
