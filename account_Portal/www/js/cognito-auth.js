// Wait for document ready and Shiny to be available
(function() {
  // Handle redirect to Cognito hosted UI
  Shiny.addCustomMessageHandler('redirectToCognito', function(url) {
    window.location.href = url;
  });

  // Store token in localStorage
  Shiny.addCustomMessageHandler('storeAuthToken', function(data) {
    localStorage.setItem('gbads_access_token', data.access_token);
    localStorage.setItem('gbads_id_token', data.id_token);
    localStorage.setItem('gbads_token_expiry', data.expiry);
  });

  // Clear token from localStorage
  Shiny.addCustomMessageHandler('clearAuthToken', function(message) {
    localStorage.removeItem('gbads_access_token');
    localStorage.removeItem('gbads_id_token');
    localStorage.removeItem('gbads_token_expiry');
  });

  // Check for stored token on page load and validate it
  function checkStoredToken() {
    const accessToken = localStorage.getItem('gbads_access_token');
    const idToken = localStorage.getItem('gbads_id_token');
    const expiry = localStorage.getItem('gbads_token_expiry');
    
    if (accessToken && idToken) {
      // Check if token is expired
      if (expiry && Date.now() < parseInt(expiry) * 1000) {
        // Token exists and is not expired, validate it with the backend
        fetch('https://gbadske.org/api/auth/profile', {
          headers: {
            'Authorization': 'Bearer ' + accessToken
          }
        })
        .then(response => {
          if (response.ok) {
            return response.json();
          } else {
            throw new Error('Token validation failed');
          }
        })
        .then(profile => {
          // Token is valid, send to Shiny
          if (typeof Shiny !== 'undefined' && Shiny.setInputValue) {
            Shiny.setInputValue('auth-stored_auth_token', {
              access_token: accessToken,
              id_token: idToken,
              profile: profile
            }, {priority: 'event'});
          }
        })
        .catch(error => {
          console.error('Token validation failed:', error);
          // Clear invalid token
          localStorage.removeItem('gbads_access_token');
          localStorage.removeItem('gbads_id_token');
          localStorage.removeItem('gbads_token_expiry');
        });
      } else {
        // Token expired, clear it
        localStorage.removeItem('gbads_access_token');
        localStorage.removeItem('gbads_id_token');
        localStorage.removeItem('gbads_token_expiry');
      }
    }
  }

  // Run when Shiny is connected
  $(document).on('shiny:connected', function() {
    checkStoredToken();
  });

  // Also try immediately if Shiny is already loaded
  if (typeof Shiny !== 'undefined') {
    setTimeout(checkStoredToken, 500);
  }
})();