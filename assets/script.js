var initAnimation = function(animEl, triggerEl, touch) {
    var hoverCallback = function() {
        var classes = (animEl.getAttribute('class') || "").split(/\s+/);
        if (classes.indexOf('animate') == -1) {
            classes.push('animate');
            animEl.setAttribute('class', classes.join(' '));
        }
    };
    if (triggerEl.addEventListener) {
        triggerEl.addEventListener('mouseover', hoverCallback);
        if (touch)
            triggerEl.addEventListener('touchstart', hoverCallback);
    } else {
        triggerEl.attachEvent('mouseover', hoverCallback);
        if (touch)
            triggerEl.attachEvent('touchstart', hoverCallback);
    }
    var animationEndCallback = function(event) {
        var target = event.target,
            classes = (target.getAttribute('class') || "").split(/\s+/),
            ind = classes.indexOf('animate');
        if (ind != -1) {
            classes.splice(ind, 1);
            target.setAttribute('class', classes.join(' '));
        }
    };
    if (animEl.addEventListener) {
        animEl.addEventListener('webkitAnimationEnd', animationEndCallback);
        animEl.addEventListener('animationend', animationEndCallback);
    } else {
        animEl.attachEvent('animationend', animationEndCallback);
    }
};

window.addEventListener('load', function() {
    initAnimation(
        document.getElementById('name'),
        document.getElementById('img_container'),
        true
    );

    initAnimation(
        document.getElementById('twitter_i'),
        document.getElementById('twitter'),
        false
    );

    initAnimation(
        document.getElementById('github_i'),
        document.getElementById('github'),
        false
    );

    initAnimation(
        document.getElementById('soundcloud_i'),
        document.getElementById('soundcloud'),
        false
    );

    initAnimation(
        document.getElementById('vim_i'),
        document.getElementById('vim'),
        false
    );
});
