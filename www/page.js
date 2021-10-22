function attachPreviewButtonListeners() {
    const buttons = document.querySelectorAll(
        "#buttons button, #buttons .button"
    );

    buttons.forEach((button) => {
        button.addEventListener("click", () => {
            toggle("#filters", false);
            toggle("#buttons", false);
            toggle("#details", true);

            toggleCollapseButtons(true);
        });
    });
}

function toggleCollapseButtons(state) {
    if (!state) {
        toggle("#filters", true);
        toggle("#buttons", true);
    }

    document.querySelector("#filters").classList.toggle("can-collapse", state);
    document.querySelector("#buttons").classList.toggle("can-collapse", state);
    document.querySelector("#details").classList.toggle("can-collapse", state);
}

function toggle(query, state) {
    /** @type {NodeListOf<HTMLElement>} */
    const elements = document.querySelectorAll(query);

    elements.forEach((element) => {
        state ??= element.classList.contains("collapsed");
        if(state) {
            element.classList.remove("collapsed");
        } else {
            element.classList.add("collapsed");
        }

        element.querySelectorAll(".collapse-toggle").forEach((toggle) => {
            toggle.innerHTML = toggle.dataset["label"] + " " + (!state ? "<span class='glyphicon glyphicon-menu-right'></span>" : "<span class='glyphicon glyphicon-menu-left'></span>");
        });
    });
}

function main() {
    console.log("page.js loaded");

    // Attach listeners to buttons for the initial page load
    attachPreviewButtonListeners();

    // Attach listeners to buttons after button filters are applied
    const observer = new MutationObserver((mutations, observer) => {
        attachPreviewButtonListeners();
    });
    observer.observe(document.querySelector("#buttons"), {
        childList: true,
        subtree: true,
        attributes: false,
    });

    // create collapse buttons
    document.querySelector("#filters .collapse-toggle")
            .addEventListener("click", () => toggle("#filters"));
    
    document.querySelector("#buttons .collapse-toggle")
            .addEventListener("click", () => toggle("#buttons"));
            
    const detailToggle = document.createElement("div");
    detailToggle.classList.add("collapse-toggle");
    detailToggle.addEventListener("click", () => {
        toggle("#details");
        toggleCollapseButtons(false);
    });
    const details = document.querySelector("#details");
    details.appendChild(detailToggle);
    //details.classList.add("collapsed");
}

$(main);
