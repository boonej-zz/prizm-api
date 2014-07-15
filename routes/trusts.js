/**
 * Handles routing & management for the Trust feature
 *
 * @author DJ Hayden <dj.hayden@stablekernel.com>
 */
var _mongoose     = require('mongoose'),
    _             = require('underscore'),
    _prism_home   = process.env.PRISM_HOME,
    _utils        = require(_prism_home + 'utils'),
    _logger       = require(_prism_home + 'logs.js'),
    PrismError    = require(_prism_home + 'error'),
    User          = require(_prism_home + 'models/user').User,
    Post          = require(_prism_home + 'models/post').Post,
    Activity      = require(_prism_home + 'models/activity').Activity,
    Twine         = require(_prism_home + 'classes/Twine'),
    PushNotification = require(_prism_home + 'classes/PushNotification'),
    Trust         = require(_prism_home + 'models/trust').Trust;

var trust_status = {
  accepted: 'accepted',
  rejected: 'rejected',
  pending: 'pending',
  canceled: 'canceled',
  inactive: 'inactive'
};

var create_trust_error = {
  status_code: 400,
  error_info: {
    error: 'unable_to_create_trust',
    error_description: 'Trust relationshop already exists'
  }
};

var update_trust_error = {
  status_code: 400,
  error_info: {
    error: 'unable_to_update_trust',
    error_description: 'Trust relationship does not exist'
  }
};

var fetch_trust_error = {
  status_code: 400,
  error_info:{
    error: 'unable_to_fetch_user_trust',
    error_description: 'The requested trust does not exist'
  }
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
        if(req.method === 'PUT' && req.body.status ||
           req.method === 'PUT' && req.body.type){
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
 */
var createTrust = function(req, res){
  validateTrustRequest(req, res, function() {
    var was_cancelled;
    var trust;
    var create_trust_error = {
      status_code: 400,
      error_info: {
        error: 'unable_to_create_trust',
        error_description: 'Trust relationshop already exists'
      }
    };

    var trust_100_error = {
      status_code: 400,
      error_info: {
        error: 'unable_to_create_trust_100_limit',
        error_description: ''
      }
    };

    var criteria = {
      $or:[ {to: req.params.id, from:req.body.creator},
            {to: req.body.creator, from:req.params.id} ]
    };

    User.find({_id: {$in: [req.params.id, req.body.creator] } }, function(err, users) {
      if(err) {
        _utils.prismResponse(res, null, false, PrismError.serverError);

      } else {
        if(users.length > 2) {
          _utils.prismResponse(res, null, false, PrismError.invalidRequest);

        } else {
          var send_trust_100_error = function(user_name) {
            trust_100_error.error_info.error_description = user_name + ' Has hit the trust limit of 100';
            _utils.prismResponse(res, null, false, trust_100_error);
          };

          if(users[0].trust_count === 100) {
            send_trust_100_error(users[0].name);
            return;
          }

          if(users[1].trust_count === 100) {
            send_trust_100_error(users[1].name);
            return;
          }
        }

        Trust.findOne(criteria, function(err, exists){
          //if an error is returned, its server related, return 500
          if(err) {
            _utils.prismResponse(res, null, false, PrismError.serverError);

          } else {
            //if a trust between 2 users already exists && the status
            //type is not equal to cancelled, then return exists error
            if(!_.isEmpty(exists) && exists.status !== 'cancelled') {
              _logger.log('error','unable to create trust',
                          {error:create_trust_error, to:req.params.id, from:req.body.creator});
              _utils.prismResponse(res, null, false, create_trust_error);
            } else if(!_.isEmpty(exists)) {
              _logger.log('info', 'updating trust status from cancelled to pending',
                          {from:req.body.creator, to:req.params.id});
              //update status to pending & save/return
              exists.status = 'pending';
              //if the requestor is not the 'FROM' user, switch reverse the order
              if(exists.from.toString() !== req.body.creator) {
                exists.to = req.params.id;
                exists.from = req.body.creator;
              }
              exists.save(function(err, updated) {
                if(err) {
                  _logger.log('error', 'error occurred trying to create new trust from cancelled',
                              {err:err});
                  _utils.prismResponse(res, false, null, PrismError.serverError);
                }
              });
            } else {
              //trust doesnt exist, create a new trust
              trust = new Trust({
                to: req.params.id,
                from: req.body.creator,
                status: 'pending'
              });
              trust.save(function(err, new_trust) {
                //if error , send server error
                if(err) {
                  _logger.log('error', 'error returned while creating new trust', {err:err});
                  _utils.prismResponse(res, null, false, PrismResponse);

                } else {
                  //TODO: create an activity? currently after the last activity refactor
                  //we were only creating an activity for trusts when someone approves/accepts it
                  //return result
                  var activity = {
                    _id: new_trust._id.toString(),
                    to: new_trust.to.toString(),
                    from: new_trust.from.toString(),
                    action: 'trust_request'
                  };

                  new PushNotification('activity', activity, function(success) {
                    _logger.log('info', 'trust request push notification', {success:success});
                  });

                  _logger.log('info', 'successful trust created from user: '+new_trust.from.toString()+
                                      ' to user: '+new_trust.toString());
                  _utils.prismResponse(res, new_trust, true);
                }
              });
            }
          }
        });
      }
    });
  });
};

/**
 * Retrieves all of the trusts for a specific user
 *
 * @param  {HTTPRequest}   req The request object
 * @param  {HTTPResponse}   res The response object
 */
var fetchTrusts = function(req, res){
  validateTrustRequest(req, res, function(){
    var criteria = {$or: [ {to: req.params.id}, {from: req.params.id} ], status: {$ne: 'inactive'}};
    new Twine('Trust', criteria, req, {fields: Trust.selectFields('basic').join(" ")}, function(err, trusts){
      var error = {
        status_code: 400,
        error_info:{
          error: 'unable_to_fetch_trusts',
          error_description: 'The requested user does not currently have any trusts'
        }
      };
      //if err is set, then it is a server / mongo error. return server error
      if(err){
        _utils.prismResponse(res, null, false, PrismError.serverError);
      // }else if(_.isEmpty(trusts)){
      //   //no trusts were returned, send unable_to_fetch_trusts error
      //   _utils.prismResponse(res, null, false, error);
      }else{
        //return normal response with result set
        _utils.prismResponse(res, trusts, true);
      }
    });
  });
};

/**
 * Retrieves a specific trust by id
 *
 * @param  {HTTPRequest}   req The request object
 * @param  {HTTPResponse}   res The response object
 */
var fetchTrustById = function(req, res){
  validateTrustRequest(req, res, function(){
    var criteria = {_id: req.params.id};
    new Twine('Trust', criteria, req, null, function(err, trust){
      if(err){
        _utils.prismResponse(res, null, false, PrismError.serverError);
      }else{
        // if(!trust){
        //   _utils.prismResponse(res, null, false, fetch_trust_error);
        // }else{
          _utils.prismResponse(res, trust, true);
        // }
      }
    });
  });
};

/**
 * Updates the specified trust (status only)
 *
 * @param  {HTTPRequest}   req The request object
 * @param  {HTTPResponse}   res The response object
 */
var updateTrust = function(req, res){
  validateTrustRequest(req, res, function(){
    Trust.findOne({_id: req.params.id})
    .populate('to from')
    .exec(function(err, trust){
      //if there is an error return server error
      if(err){
        _logger.log('error', 'fetching trust to update failed with error', {err:err});
        _utils.prismResponse(res, null, false, PrismError.serverError);
      }else{
        if(_.isEmpty(trust)){
          //trust is empty, return unable to find trust error
          _logger.log('error', 'unable to find existing trust to update status');
          _utils.prismResponse(res, null, false, update_trust_error);
        }else{
          //update trust status
          if(!_.isUndefined(req.body.type)){
            trust.type = req.body.type;
          }

          if(!_.isUndefined(req.body.status)){
            trust.status = req.body.status;
          }

          trust.save(function(err, trust_updated){
            if(err){
              _logger.log('error', 'updating trust status failed with error', {err:err});
              _utils.prismResponse(res, null, false, PrismError.serverError);

            }else{
              //if status update is accepted, create activity. remember that you need to
              //inverse the to/from since the action is being executed against the creator
              //who WAS the `from` user & now is the `to` user. this is necessary so that
              //when a users trusts are fetched this action shows up for the requestor and
              //not the requestee
              if(req.body.status === 'accepted'){

                _utils.registerActivityEvent(trust.from, trust.to, 'trust_accepted');

                if(trust.from.type === 'institution_verified' && trust.from.subtype && trust.from.subtype !== 'luminary') {
                  trust.to.subtype = 'luminary';
                  trust.to.save(function(err, to_saved) {
                    console.log("trust.to.save error: "+ JSON.stringify(err));
                    console.log("trust.to.save result: " + JSON.stringify(to_saved));
                    if(err) {
                      _utils.prismResponse(res, null, false, PrismError.serverError);
                      //reset trust to pending?

                    }

                    trust_updated.updateUsersTrustCount(function(err){
                      if(err) {
                        _logger.log('error', 'Update users trust count Error from within update luminary', {error:err});
                        _utils.prismResponse(res, null, false, PrismError.serverError);
                        return;
                      }

                      Post.updateSubtypeToLuminary(trust.to._id.toString(), function(err, post_updated) {
                        if(err) {
                          _logger.log('error', 'Unable to update posts subtype to luminary', {error:err});
                          _utils.prismResponse(res, null, false, PrismError.serverError);
                          return;
                        }

                        _utils.prismResponse(res, trust_updated, true);
                      });
                    });

                  });

                } else {

                  trust_updated.updateUsersTrustCount(function(err){
                    if(err){
                      _logger.log('error',
                                  'Update Users Trust Count ERROR!',
                                  {error: err});
                      _utils.prismResponse(res, null, false, PrismError.serverError);
                      return;
                    }

                    _utils.prismResponse(res, trust_updated, true);
                  });
                }
              }
                _utils.prismResponse(res, trust_updated, true);
            }
          });
        }
      }
    });
  });
};

/**
 * Checks to see if a trust between 2 users exists
 *
 * @param  {HTTPRequest}   req The request object
 * @param  {HTTPResponse}   res The response object
 */
var exists = function(req, res){
  if(req.params.id && req.params.user_id){
    var criteria = {
      $or:[ {to: req.params.id, from:req.params.user_id},
            {to: req.params.user_id, from:req.params.id} ]
    };
    Trust.findOne(criteria, function(err, found){
      if(err) _utils.prismReponse(res, null, false, PrismError.serverError);
      // if(_.isEmpty(found)){
        // var error = {
        //   status_code: 400,
        //   error_info: {
        //     error: 'unable_to_find_existing_trust',
        //     error_description: 'An existing trust does not currently exist between '+
        //                         req.params.id + ' and '+ req.params.user_id
        //   }
        // };
        // _utils.prismResponse(res, null, false, error);
      // }else{
        _utils.prismResponse(res, found, true);
      // }
    });
  }else{
    _utils.prismResponse(res, null, false, PrismError.invalidRequest);
  }
};

var deleteTrust = function(req, res){
  validateTrustRequest(req, res, function(){
    res.send('Not fully implemented yet');
  });
};

/**
 * Searches for user names within the specificed users trusts
 *  Note: trust must be accpeted
 *
 * @param  {HTTPRequest}   req The request object
 * @param  {HTTPResponse}   res The response object
 */
var searchForUsersInTrust = function(req, res){
  if(req.params.id && req.params.name){
    //first gather an array of user id's that exist in the specified users trusts
    Trust.find(
      {$or: [{to: req.params.id}, {from: req.params.id}], status: 'accepted'},
      {to: 1, from: 1},
      function(error, trusts){
        if(error){
          //server error
          _logger.log('error',
                      'Error returned trying to fetch trusts for user search',
                      {error: error});
          //return server error in response
          _utils.prismResponse(res, null, false, PrismError.serverError);

        }else if(!trusts){
          //nothing was returned, user has no trusts, send fetch error
          _utils.prismResponse(res, null, false, fetch_trust_error);

        }else{
          if(trusts.length === 0){
            _utils.prismResponse(res, null, false, fetch_trust_error);

          }else{
            var users = [];
            var iterate = function(item){
              if(item.to.toString() === req.params.id)
                users.push(item.from);
              if(item.from.toString() === req.params.id)
                users.push(item.to);
            };

            _.each(trusts, iterate);

          //now search users & return short user if found

          //regex name search characters globally & case incensitive
          var regex_name = new RegExp(req.params.name, 'gi');

          //fetch users in array that match the regex
          User.find({_id: {$in: users}, name: regex_name})
          .select(User.selectFields('short').join(" "))
          .exec(function(error, users_result){
            if(error){
              _logger.log('error',
                          'An error was returned while searching names in trusts',
                          {error: error});
              //server error return issue
              _utils.prismResponse(res, null, false, PrismError.serverError);

            }else{
              _utils.prismResponse(res, users_result, true);
            }
          });
        }
      }
    });
  }else{
    _utils.prismResponse(res, null , false, PrismError.invalidRequest);
  }
};

/**
 * Searches for usernames that the specified user currently does
 * not have a trust with.
 *
 * @param {HTTPRequest} req The request object
 * @param {HTTPResponse} res The response object
 */
var searchForUsersNotInTrust = function(req, res){
  Trust.find(
    {$or: [{to: req.params.id}, {from: req.params.id}], status: 'accepted'},
    {to:1, from:1},
    function(err, trusts){
      if(err){
        _logger.log('error',
                    'There was an error returned while fetching users trusts',
                    {err:err});
        _utils.prismResponse(res, null, false, PrismError.serverError);

      }else{
        var users = [];
        var iterate = function(item){
          if(item.to.toString() === req.params.id)
            users.push(item.from);
          if(item.from.toString() === req.params.id)
            users.push(item.to);
        };

        _.each(trusts, iterate);

        //now search users & return short user if found

        //regex name search characters globally & case incensitive
        var regex_name = new RegExp(req.params.name, 'gi');

        User.find({_id: {$nin: users}, name: regex_name})
        .select(User.selectFields('short').join(" "))
        .exec(function(error, users_result){
          if(error){
            _logger.log('error',
                        'An error returned while searching names in users',
                        {error:error});
            _utils.prismResponse(res, null, false, PrismError.serverError);
          }else{
            _utils.prismResponse(res, users_result, true);
          }
        });
      }
    });
};

var thisExports = {
  createTrust: createTrust,
  fetchTrusts: fetchTrusts,
  updateTrust: updateTrust,
  deleteTrust: deleteTrust,
  exists:      exists,
  fetchTrustById: fetchTrustById,
  searchForUsersInTrust: searchForUsersInTrust,
  searchForUsersNotInTrust: searchForUsersNotInTrust
};

if(process.env.NODE_ENV === 'test'){
  thisExports.validateTrustRequest = validateTrustRequest;
}

module.exports = thisExports;


