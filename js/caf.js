$(document).on('click', '.itemID', function () {
  Shiny.onInputChange('last_btn',this.id);
});