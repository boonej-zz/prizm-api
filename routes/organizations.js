var mongoose      = require('mongoose');
var home          = process.env.PRISM_HOME;
var utils         = require(home + 'utils');
var PrizmError    = require(home + 'error');
var Twine         = require(home + 'classes/Twine');
var Organization  = mongoose.model('Organization');
var Theme         = mongoose.model('Theme');
var User          = mongoose.model('User');
var _ = require('underscore');

exports.searchOrganizations = function(req, res){
  var code = req.params.code;
  new Twine('Organization', {code: code}, req, null, function(err, result){
    if (err){
      console.log('error', 'Fetching organization returned error.');
      res.send(500);
    } else {
      console.log(result);
      utils.prismResponse(res, result, true);
    }
  });
};
