var mongoose   = require('mongoose'),
    prism_home = process.env.PRISM_HOME,
    utils      = require(prism_home + 'utils'),
    logger     = require(prism_home + 'logs.js'),
    PrismError  = require(prism_home + 'error'),
    Twine       = require(prism_home + 'classes/Twine'),
    Interest    = mongoose.model('Interest');

exports.createInterest = function(req, res){
  console.log('creating interest');
  if (req.body.text) {
    var interest = new Interest({
      text: req.body.text
    });
    if (req.body.subinterests){
      for (var i = 0; i != req.body.subinterests.length; ++i){
        si = req.body.subinterests[i];
        if (si.text){
          var subinterest = new Interest({
            text : si.text,
            is_subinterest: true
          });
          subinterest.save();
          interest.subinterests.push(subinterest._id);
        } 
      }
    }
    interest.save(function(err, result){
      if (err) {
        console.log(err);
        utils.prismResponse(res, null, false, PrismError.invalidRequest);
      }
      res.send('success');
    });
  } else {
    res.send('error');
  }
};

exports.fetchInterests = function(req, res){
  //console.log('fetching interests');
  // criteria = {is_subinterest: false};
  criteria = [];
  new Twine('Interest', criteria, req, null, function (error, result){
    if (error){
      console.log(error);
      utils.prismResponse(res, null, false, PrismError.serverError);
    }
    utils.prismResponse(res, result, true);
  });

};

