const showFeedback = function(show, delay = 0) {
  const feedback = document.getElementsByClassName("feedback");
  Array.from(feedback).forEach(function (f) {
    f.style.transition = "opacity " + delay + "s";
    f.style.opacity = show ? 1 : 0;
  });
};

const flashFeedback = function() {
  setTimeout(function() {
    showFeedback(false, 0.25);
  }, 500);
};

document.addEventListener("guessMade", function() {
  flashFeedback();
});
