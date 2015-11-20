exports.invalidRequest = function(res, desc){
  var error = {error: 'invalid_request'};
  if (desc) {
    error.description = desc;
  }
  res.status(400).json(error);
};

exports.serverError = function(res, desc) {
  var error = {
    error: 'server_error' 
  };
  error.description = desc || 'There was a problem processing your request.';
  res.status(500).json(error);
};
