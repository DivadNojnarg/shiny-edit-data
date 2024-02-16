$(function() {
  // Needed to compute the diff for cell rendering.
  // This contains the very first data version.
  Shiny.addCustomMessageHandler('send-init-data', function(m) {
    initData = m.value;
  });
  // Correct initial state for buttons
  Shiny.addCustomMessageHandler('toggle-buttons', function(m) {
    for (let i = 0; i < m.value.length; i++) {
      let target = $('#edit-table').find('button')[i];
      $(target).prop('disabled', m.value[i]);
    }
  });

  Shiny.addCustomMessageHandler("can-save", function(m) {
    $('#edit-update_row').prop('disabled', !m.value);
  })
});
