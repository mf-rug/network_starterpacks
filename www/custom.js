// Document ready event handler to initialize the script
$(document).ready(function() {
  // Event handler for mouse enter on avatar buttons to show tooltip
  $(document).on('mouseenter', '.avatar-btn', function() {
    $(this).siblings('.avatar-tooltip').addClass('visible'); // Add visible class to sibling tooltip
  });
  
  // Event handler for mouse leave on avatar buttons to hide tooltip
  $(document).on('mouseleave', '.avatar-btn', function() {
    $(this).siblings('.avatar-tooltip').removeClass('visible'); // Remove visible class from sibling tooltip
  });
});

// Another document ready event handler to initialize the script
$(document).ready(function() {
  // Event handler for click on avatar buttons to send data to Shiny
  $(document).on('click', '.avatar-btn', function() {
    var btnId = $(this).attr('id'); // Get the ID attribute of the button
    var dataId = $(this).attr('data-id'); // Get the custom data-id attribute of the button

    // Send both values to Shiny with priority as event
    Shiny.setInputValue('clicked_avatar', { btnId: btnId, dataId: dataId }, { priority: 'event' });
  });
});