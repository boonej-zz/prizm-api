/**
 * Handles routing & management for the Trust feature
 *
 * @author DJ Hayden <dj.hayden@stablekernel.com>
 */
var _mongoose     = require('mongoose'),
    _prism_home   = process.env.PRISM_HOME,
    _utils        = require(_prism_home + 'utils'),
    _logger       = require(_prism_home + 'logs.js'),
    PrismError    = require(_prism_home + 'error'),
    User          = require(_prism_home + 'models/user').User,
    Post          = require(_prism_home + 'models/post').Post,
    Trust         = require(_prism_home + 'models/user').Trust;

var trust_status = {
  accepted: 'accepted',
  rejected: 'rejected',
  pending: 'pending',
  canceled: 'canceled'
};

/**
 * Request validation helper method to ensure minimum request params are included
 *
 * @param  {HTTPRequest}   req The request object
 * @param  {HTTPResponse}   res The response object
 * @param  {Function} cb  The callback function invoked when all criteria is met
 */
var validateTrustRequest = function(req, res, cb){
  var body_required = ['POST', 'PUT', 'DELETE'];
  var invalidRequest = function(){
    _utils.prismResponse(res, null, false, PrismError.invalidRequest);
  };

  if(req.params.id){
    if(body_required.indexOf(req.method) === -1){
      cb();
    }else{
      if(req.method === 'PUT' || req.method === 'DELETE'){
        if(!req.params.trust_id){
          invalidRequest();
          return;
        }

        if(req.method === 'PUT' && req.body.status){
          cb();
        }else if(req.method === 'DELETE' && req.body.creator){
          cb();
        }else{
          invalidRequest();
        }

      }else{
        if(req.body.creator){
          cb();
        }else{
          invalidRequest();
        }
      }
    }
  }else{
    invalidRequest();
  }
};

/**
 * Initiates a trust relationship between 2 users
 *
 * @param  {HTTPRequest}   req The request object
 * @param  {HTTPResponse}   res The response object
 * @return {}
 */
var createTrust = function(req, res){
  validateTrustRequest(req, res, function(){
    User.find({_id: {$in : [req.params.id, req.body.creator]}}, function(err, users){
      if(err){
        _utils.prismResponse(res, null, false, PrismError.serverError);
      }else{
        if(!users || users.length !== 2){
          _utils.prismResponse(res, null, false, PrismError.invalidUserRequest);
        }else{
          var sender, receiver;
          if(req.params.id === users[0]._id.toString()){
            receiver = users[0];
            sender = users[1];
          }else{
            receiver = users[1];
            sender = users[0];
          }

          var was_cancelled = sender.previousTrustCancelled(receiver._id.toString());
          if(was_cancelled && was_cancelled >= 0){
            var send_index = sender.fetchTrustIndexByUserId(receiver._id.toString());
            if(send_index){
              sender.trusts[i].status = trust_status.pending;
              sender.save(function(err, send_saved){
                if(err){
                  _utils.prismResponse(res, null, false, PrismError.serverError);
                }else{
                  var rec_index = receiver.fetchTrustIndexByUserId(sender._id.toString());
                  receiver.trusts[i].status = trust_status.pending;
                  sender.save(function(err, rec_saved){
                    var rec_response = null;
                    rec.trusts.forEach(function(trust){
                      if(trust._id === receiver.trusts[i]._id){
                        rec_response = trust.toObject;
                        rec_response.user_id = sender.shortUser();
                      }
                    });
                    if(rec_response){
                      _utils.prismResponse(res, rec_response, true);
                    }else{
                      _utils.prismResponse(res, null, false, PrismError.serverError);
                    }
                  });
                }
              });
            }
          }else if(!receiver.doesTrustExist(sender._id.toString()) &&
            !sender.doesTrustExist(receiver._id.toString())){
            //set the trust object for the receiver

            var trust_receiver = new Trust({
              user_id: sender._id,
              status: trust_status.pending,
              is_owner: false
            });

            receiver.trusts.push(trust_receiver);
            receiver.trusts_count++;

            //set the trust object for the sender
            var trust_sender = new Trust({
              user_id: receiver._id,
              status: trust_status.pending,
              is_owner: true
            });
            sender.trusts.push(trust_sender);
            sender.trusts_count++;

            //update & save receiver object
            receiver.save(function(err, rec){
              if(err || !rec){
                _utils.prismResponse(res, null, false, PrismError.serverError);
              }else{
                //update & save sender object
                sender.save(function(err, send){
                  if(err || !send){
                    _utils.prismResponse(res, null, false, PrismError.serverError);
                  }else{
                    var rec_response = null;
                    rec.trusts.forEach(function(trust){
                      if(trust._id === trust_receiver._id){
                        rec_response = trust.toObject();
                        rec_response.user_id = sender.shortUser();
                      }
                    });

                    if(rec_response) {
                      _utils.prismResponse(res, rec_response, true);
                    }else{
                      _utils.prismResponse(res, null, false, PrismError.serverError);
                    }
                  }
                });
              }
            });
          }else{
            var error = {
              status_code: 400,
              error_info: {
                error: 'unable_to_create_trust',
                error_description: 'Trust relationshop already exists'
              }
            };
            _utils.prismResponse(res, null, false, error);
          }
        }
      }
    });
  });
};

/**
 * Retrieves all of the trusts for a specific user
 *
 * @param  {HTTPRequest}   req The request object
 * @param  {HTTPResponse}   res The response object
 * @return {}
 */
var fetchTrusts = function(req, res){
  validateTrustRequest(req, res, function(){
    var criteria = {_id: req.params.id};

    //check to see if status filter is set
    // if(typeof(req.query.status) !== 'undefined'){
    //   // criteria["trusts.status"] = req.query.status;
    //   criteria = {_id: req.params.id, "trusts.status": req.query.status};
    // }

    // //check to see if owner filter is set
    // if(typeof(req.query.owner) !== 'undefined'){
    //   criteria["trusts.owner"] = req.query.owner;
    // }

    var fetch = User.findOne(criteria);
    fetch.populate('trusts.user_id', '_id name first_name last_name profile_photo_url');
    fetch.exec(function(err, result){
      if(err){
        _utils.prismResponse(res, null, false, PrismError.serverError);
      }else{
        if(!result || result.trusts_count === 0){
          var error = {
            status_code: 400,
            error_info:{
              error: 'unable_to_fetch_trusts',
              error_description: 'The requested user does not currently have any trusts'
            }
          };
          _utils.prismResponse(res, null, false, error);
        }else{
          var filtered_trusts = [];
          if(typeof(req.query.status) !== 'undefined'){
            for(var i =0; i < result.trusts.length; i++){
              if(result.trusts[i].status === req.query.status){
                filtered_trusts.push(result.trusts[i]);
              }
            }
          }

          if(typeof(req.query.owner) !== 'undefined'){
            for(var o = 0; o < result.trusts.length; o++){
              if(result.trusts[o].is_owner.toString() === req.query.owner){
                filtered_trusts.push(result.trusts[o]);
              }
            }
          }
          if(filtered_trusts.length === 0) filtered_trusts = result.trusts;
          _utils.prismResponse(res, {trusts_count: result.trusts_count, trusts: filtered_trusts}, true);
        }
      }
    });
  });
};

var updateTrust = function(req, res){
  validateTrustRequest(req, res, function(){
    User.findOne({_id: req.params.id})
      .populate('trusts.user_id', '_id name first_name last_name profile_photo_url')
      .exec(function(err, user){
      var index;
      var short_user;
      for(var i = 0; i < user.trusts.length; i++){
        if(user.trusts[i]._id.toString() == req.params.trust_id.toString()){
          index = i;
          user.trusts[i].status = req.body.status;
          short_user = user.trusts[i].user_id;
        }
      }

      if(index >= 0){
        if(user.trusts[index].user_id._id !== user._id && !req.body.status){
          var self_update_error = {

          };
          _utils.prismResponse(res, null, false, self_update_error);

        }else{
          user.save(function(err, saved){
            if(err){
              _utils.prismResponse(res, null, false, PrismError.serverError);

            }else{
              //update requestors status as well.
              User.findOne({_id: user.trusts[index].user_id._id}, function(err, requestor){
                var found = false;
                for(var i=0; i < requestor.trusts.length; i++){
                  if(requestor.trusts[i].user_id.toString() === user._id.toString()){
                    requestor.trusts[i].status = req.body.status;
                    found = true;
                  }
                }
                if(!found){
                  _utils.prismResponse(res, null, false, PrismError.severError);

                }else{
                  requestor.save(function(err){

                    if(err){
                      _utils.prismResponse(res, null, false, PrismError.severError);

                    }else{
                      _utils.prismResponse(res, saved.trusts[index], true);

                    }
                  });
                }
              });
            }
          });
        }
      }else{
        _utils.prismResponse(res, null, false, PrismError.invalidRequest);
      }
    });
    // res.send('Not fully implemented yet');
  });
};

var deleteTrust = function(req, res){
  validateTrustRequest(req, res, function(){
    res.send('Not fully implemented yet');
  });
};

var thisExports = {
  createTrust: createTrust,
  fetchTrusts: fetchTrusts,
  updateTrust: updateTrust,
  deleteTrust: deleteTrust
};

if(process.env.NODE_ENV === 'test'){
  thisExports.validateTrustRequest = validateTrustRequest;
}

module.exports = thisExports;


