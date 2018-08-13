$(document).on('click', '.itemID', function () {
  Shiny.onInputChange('last_btn',this.id);
   var bisabuelo = this.parentNode.parentNode.parentNode
  var button = bisabuelo.querySelector('button')
  var isActive = document.querySelector('.butTemas.active')
  if (isActive) {
    isActive.classList.remove('active')
  }
  button.classList.add('active')
});