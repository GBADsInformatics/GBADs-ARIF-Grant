// Handle redirect to Cognito hosted UI
Shiny.addCustomMessageHandler('redirectToCognito', function(url) {
  window.location.href = url;
});