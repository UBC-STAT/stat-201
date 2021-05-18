function load_page(path) {
    const page = path.split("#")[0];
    const section = path.split("#")[1];

    console.log(`path: ${path}`);
    console.log(`page: ${page}`);
    console.log(`section: ${section}`);

    fetch(page)
        .then(response => response.text())
        .then(text => {
            document.querySelector('main').innerHTML = text;
            if (section) {
                document.getElementById(section).scrollIntoView(true);
            }
        });
}

document.addEventListener('DOMContentLoaded', () => {
    document.querySelectorAll('[data-page]').forEach(link => {
        link.addEventListener("click", () => {
            console.log(link.dataset.page);
            load_page(link.dataset.page);
        });
    });
});
