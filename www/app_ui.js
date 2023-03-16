
// Trigger the click event on the default active tab after the Shiny app is loaded
// so that renderPlot or renderPlotly have the value of the selected tab on startup
// CLICK THE DEFAULT TAB ON STARTUP
$(document).on('shiny:connected', function () {
    $('#nav_vacc_map_tab').trigger('click');
});

last_tab_state = "nav_vacc_map_tab"
remembered_checkbox_state = false

function remember_checkbox_checked_state(){
    remembered_checkbox_state = document.getElementById('is_scale_adaptive').checked
}
function set_adaptive_checkbox_enabled(state){
    document.getElementById('is_scale_adaptive').disabled = !state
}
function set_checkbox_checked_state(checked) {
    return document.getElementById('is_scale_adaptive').checked = checked
}

// Keep track of the current tab and the state of the adaptive scale checkbox
document.getElementById('nav_vacc_map_tab').addEventListener('click', function () {

    // Disable and Enable "Scale Adaptive" checkbox
    set_adaptive_checkbox_enabled(true)
    set_checkbox_checked_state(remembered_checkbox_state)

    Shiny.setInputValue('active_tab', 'nav_vacc_map_tab');
    last_tab_state = "nav_vacc_map_tab"
});

document.getElementById('nav_hosp_plot_tab').addEventListener('click', function () {
    if (last_tab_state = "nav_vacc_map_tab")
        remember_checkbox_checked_state()

    set_adaptive_checkbox_enabled(false)
    set_checkbox_checked_state(true)
    Shiny.setInputValue('active_tab', 'nav_hosp_plot_tab');
    last_tab_state = "nav_hosp_plot_tab"
});

document.getElementById('nav_about_tab').addEventListener('click', function () {
    if (last_tab_state = "nav_vacc_map_tab")
        remember_checkbox_checked_state()

    set_checkbox_checked_state(false)
    set_adaptive_checkbox_enabled(false)
    Shiny.setInputValue('active_tab', 'nav_about_tab');
    last_tab_state = "nav_about_tab"
});



