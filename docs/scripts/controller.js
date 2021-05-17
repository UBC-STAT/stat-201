function load_page(name) {
    const request = new XMLHttpRequest();
    request.open('GET', name);
    request.responseType = 'document';
    request.onload = () => {
        const response = request.responseXML;
        document.querySelector('main').innerHTML = response.querySelector('main').innerHTML;
        console.log(request.response.querySelector('main').innerHTML);
    }
    request.send();
};

document.addEventListener('DOMContentLoaded', () => {
    document.querySelectorAll('[data-page]').forEach(link => {
        link.onclick = () => {
            load_page(link.dataset.page);
            return false;
        };
    });
});