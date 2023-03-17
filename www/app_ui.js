
// Trigger the click event on the default active tab after the Shiny app is loaded
// so that renderPlot or renderPlotly have the value of the selected tab on startup
// CLICK THE DEFAULT TAB ON STARTUP
default_tab = "nav_vacc_map_tab"

$(document).on('shiny:connected', function () {
    // VALUE OF MAP SIZE CANNOT BE NULL ON STARTUP
    Shiny.setInputValue('size_slider_value', 350)   // set before clicking
    Shiny.setInputValue('active_tab', default_tab);       // open default tab
});



// ENABLE AND DISABLE SHARED MAP CONTROLS
last_tab_state = default_tab
remembered_checkbox_state = false

function remember_checkbox_checked_state(){
    remembered_checkbox_state = document.getElementById('is_scale_adaptive').checked
}
function set_adaptive_checkbox_enabled(state){
    document.getElementById('is_scale_adaptive').disabled = !state
}
function set_interactive_checkbox_enabled(state) {
    return document.getElementById('is_plot_dynamic').disabled = !state
}

function set_adaptive_checkbox_checked(checked) {
    document.getElementById('is_scale_adaptive').checked = checked
}
function set_interactive_checkbox_checked(checked) {
    return document.getElementById('is_plot_dynamic').checked = checked
}

// TRIGGER SELECTED TAB ON CLICK

// Keep track of the current tab and the state of the adaptive scale checkbox
document.getElementById('nav_vacc_map_tab').addEventListener('click', function () {
    set_interactive_checkbox_enabled(true)
    set_adaptive_checkbox_enabled(true)
    Shiny.setInputValue('active_tab', 'nav_vacc_map_tab');
});

document.getElementById('nav_hosp_plot_tab').addEventListener('click', function () {
    set_interactive_checkbox_enabled(true)
    set_adaptive_checkbox_enabled(false)
    set_adaptive_checkbox_checked(true)
    Shiny.setInputValue('active_tab', 'nav_hosp_plot_tab');
});

document.getElementById('nav_wp_map_tab').addEventListener('click', function () {
    set_interactive_checkbox_enabled(false)
    set_adaptive_checkbox_enabled(false)
    set_interactive_checkbox_checked(false)
    set_adaptive_checkbox_checked(false)
    Shiny.setInputValue('active_tab', 'nav_wp_map_tab');
});

document.getElementById('nav_about_tab').addEventListener('click', function () {
    set_interactive_checkbox_enabled(false)
    set_adaptive_checkbox_enabled(false)
    Shiny.setInputValue('active_tab', 'nav_about_tab');
});

// TRIGGER SLIDER
document.getElementById('graph_size_slider').addEventListener('mouseup', function () {
    Shiny.setInputValue('size_slider_value', this.value);
});