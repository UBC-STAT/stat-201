// Add data to the current page history
//history.replaceState({ page: "index.html" }, "", "index.html")

// load the page with the back button
window.onpopstate = function (event) {
    load_page(event.state.page);
}

// loading the main function
function load_page(path) {
    const page = path.split("#")[0];
    const section = path.split("#")[1];

    fetch(page)
        .then(response => response.text())
        .then(text => {
            const parser = new DOMParser();
            const html = parser.parseFromString(text, "text/html");
            text = html.querySelector("main").innerHTML;
            document.querySelector('main').innerHTML = text;
            if (section) {
                document.getElementById(section).scrollIntoView({ block: "center", inline: "nearest" });
            }
            else {
                document.body.scrollTop = 0;
                document.documentElement.scrollTop = 0;
            }

        });
}

// add event listener to load the pages.
document.addEventListener('DOMContentLoaded', () => {
    document.querySelectorAll('[data-page]').forEach(link => {
        link.onclick = (event) => {
            load_page(link.dataset.page);
            history.pushState({ page: link.dataset.page }, "", link.dataset.page)
            return false;
        };
    });
});

