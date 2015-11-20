exports.invalidRequest = function(res, desc){
  var error = {error: 'Invalid request'};
  if (desc) {
    error.description = desc;
  }
  res.status(400).json(error);
};

exports.serverError = function(res) {
  var error = {
    error: 'Server error', 
    description: 'There was a problem processing your request.'
  };
  res.status(500).json(error);
};
