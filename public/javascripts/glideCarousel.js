document.addEventListener('DOMContentLoaded', function () {
    new Glide('.glide', {
        type: 'carousel',
        perView: 1,  // Show one image at a time
        focusAt: 'center',
        autoplay: 4000,  // Autoplay speed in milliseconds
    }).mount();
});
