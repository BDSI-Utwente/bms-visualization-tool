shinyjs.sortButtons = function (params) {
    const { hide } = shinyjs.getParams(params, { hide: false });
    const container = $("#buttons-content");
    const buttons = container.children();
    const sorted = buttons.sort((a, b) => {
        if (a.disabled != b.disabled) return a.disabled ? 1 : -1;
        return a.id.localeCompare(b.id);
    });
    if (hide) {
        for (const button of buttons) {
            if (button.disabled) {
                $(button).hide();
            } else {
                $(button).show();
            }
        }
    }
    container.append(sorted);
};
