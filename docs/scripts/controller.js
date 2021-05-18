function load_page(name, section) {
    const request = new XMLHttpRequest();
    request.open('GET', name);
    request.responseType = 'document';
    request.onload = () => {
        const response = request.responseXML;
        document.querySelector('main').innerHTML = response.querySelector('main').innerHTML;
        console.log(section);
        if (section !== ""){
            document.getElementById(section).scrollIntoView(true);
        }
    };
    request.send();
};

document.addEventListener('DOMContentLoaded', () => {
    document.querySelectorAll('[data-page]').forEach(link => {
        const page = link.dataset.page.split("#")[0];
        const section = link.dataset.page.split("#")[1];
        link.onclick = () => {
            load_page(page, section);
            return false;
        };
    });
});