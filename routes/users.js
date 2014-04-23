/**
 * Handles routing & management for /user* endpoints
 *
 * @author DJ Hayden <dj.hayden@stablekernel.com>
 */
var _mongoose     = require('mongoose'),
    _             = require('underscore'),
    _uuid         = require('node-uuid'),
    _prism_home   = process.env.PRISM_HOME,
    _utils        = require(_prism_home + 'utils'),
    _logger       = require(_prism_home + 'logs'),
    PrismError    = require(_prism_home + 'error'),
    Facebook      = require(_prism_home + 'classes/Facebook'),
    Twitter       = require(_prism_home + 'classes/Twitter'),
    Google        = require(_prism_home + 'classes/Google'),
    User          = require(_prism_home + 'models/user').User,
    Twine         = require(_prism_home + 'classes/Twine'),
    Trust         = require(_prism_home + 'models/trust').Trust,
    Activity      = require(_prism_home + 'models/activity'),
    Post          = require(_prism_home + 'models/post').Post,
    Mail          = require(_prism_home + 'classes/Mail');

/**
 * TODO: pull logging for errors out into error class (which needs refactoring)
 */

/*jshint -W087*/

/**
 * Handles User review state
 *
 * @param {HTTPRequest} req The request object
 * @param {HTTPResponse} res The response object
 */
exports.review = function review(req, res){
  if(req.params.id && req.params.review){
    if(req.query.review_key && req.query.approval){
      User.findOne({_id: req.params.id}, function(err, user){
        if(err){
          _utils.prismResponse(res, null, false, PrismError.serverError);

        }else{
          if(user.review_key === req.query.review_key &&
             user.status === 2){
            user.type = (req.query.approval === 'yes') ? 'institution' : 'user';
            user.review_key = null;
            user.status = 0;
            user.save(function(err, saved){
              if(err){
                _utils.prismResponse(res, null, false, PrismError.serverError);

              }else{
                res.send('Succesfully Reviewed User');
              }
            });
          }else{
            res.send('Unable to review user. User is currently not in review');
          }
        }
     });
    }else{
      _utils.prismResponse(res, null, false, PrismError.invalidRequest);
    }
  }else{
    _utils.prismResponse(res, null, false, PrismError.invalidRequest);
  }
};

/**
 * Handles authenticating user login request
 *
 * TODO: create session on success & add logout to destroy session
 *
 * @param  {HTTPRequest} req The request object
 * @param  {HTTPResponse} res The response object
 * @return {User} Returns a valid user object
 */
exports.login = function(req, res){
  if(isValidLoginRequest(req.body)){
    if(isSocialProvider(req.body)){
      handleSocialProviderLogin(req.body, function(error, result){
        if(error || result === false){
          //social login failure - user does not exist or
          //failire to authenticate with social provider
          if(error){
            _utils.prismResponse( res,
                                  null,
                                  false,
                                  error);
          }else{
            _utils.prismResponse( res, null, false, PrismError.invalidLoginUserDoesNotExist);
          }
        }else{
          //succesful login - send back returned user object
          _utils.prismResponse( res, result, true);
        }
      });
    }else{
      var user = User.findOne({email: req.body.email});
      user.select(User.selectFields('internal').join(" "));
      user.exec(function(error, result){
        if(error){
          _utils.prismResponse( res,
                                null,
                                false,
                                PrismError.invalidLoginUserDoesNotExist);
        }else if(result){
          if(hashAndValidatePassword(result, req.body.password)){
            _utils.prismResponse(res, result.format('basic'), true, null, null);
          }else{
           _utils.prismResponse(res,
                                null,
                                false,
                                PrismError.invalidUserCredentials);
          }
        }else{
          _utils.prismResponse( res,
                                null,
                                false,
                                PrismError.invalidLoginUserDoesNotExist);
        }
      });
    }
  }else{
    _utils.prismResponse( res,
                          null,
                          false,
                          PrismError.invalidLoginRequest);
  }
};

/**
 * Handles User creation if user does not exist
 *
 * @param  {HTTPRequest} req The request object
 * @param  {HTTPResponse} res The response object
 * @return {User} Returns the newly created User object
 */
exports.register = function(req, res){
  if(isValidRegisterRequest(req)){
    //Handle traidtional registration --
    var newUser = new User({
      first_name: req.body.first_name,
      last_name: req.body.last_name,
      email: req.body.email,
      gender: req.body.gender,
      zip_postal: req.body.zip_postal,
      city: req.body.city,
      state: req.body.state,
      birthday: req.body.birthday
    });

    if(typeof(req.body.password) != 'undefined') newUser.password = req.body.password;

    if(typeof(req.body.cover_photo_url) != 'undefined') newUser.cover_photo_url = req.body.cover_photo_url;

    if(typeof(req.body.profile_photo_url) != 'undefined') newUser.profile_photo_url = req.body.profile_photo_url;
    if(typeof(req.body.type) !== 'undefined') newUser.type = req.body.type;
    if(newUser.type === 'institution'){
      newUser.status = 2;
      newUser.type = 'user';
      newUser.review_key = _uuid.v1();
    }

    //check, validate, & handle social registration
    if(isSocialProvider(req.body)){
      handleSocialProviderRegistration(req.body, function(error, social){
        if(error && social === false){
          _utils.prismResponse( res, null, false, error, PrismError.status_code);
        }else if(social){
          newUser.provider = req.body.provider;
          newUser.provider_token = req.body.provider_token;
          newUser.provider_id = social.id;
          if(newUser.provider == 'twitter'){
            newUser.provider_token_secret = req.body.provider_token_secret;
          }
          newUser.save(function(error, result){

            if(error || !result){
               _utils.prismResponse( res,
                                null,
                                false,
                                PrismError.invalidRegisterUserExists,
                                PrismError.invalidRegisterUserExists.status_code);
            }else{
              if(result.review_key && result.status === 2){
                var mail = new Mail();
                mail.reviewInstitution(result);
              }

              _utils.prismResponse(res, result.format('basic'), true);
            }

          });
        }else{
          _utils.prismResponse( res, null, false, PrismError.serverError, PrismError.serverPrismError.status_code);
        }
      });
    }else{

      newUser.save(function(error, result){
        if(error || !result){
          _utils.prismResponse( res,
                                null,
                                false,
                                PrismError.invalidRegisterUserExists,
                                PrismError.invalidRegisterUserExists.status_code);
        }else{
          if(result.review_key && result.status === 2){
            var mail = new Mail();
            mail.reviewInstitution(result);
          }

          _utils.prismResponse(res, result.format('basic'), true);
        }
      });
    }

  }else{
    _utils.prismResponse(res, null, false, PrismError.invalidRequest, PrismError.invalidRequest.status_code);
  }
};

var formatStringSearchVariable = function(search_string){
  return new RegExp(search_string, "i");
};

/**
 * Fetchs all users.
 *
 * //TODO: this should be moved to a middleware or global object
 * currently checks for user's name in the query object & trys to apply the filter
 * to the query criteria
 *
 * @param  {[type]} req [description]
 * @param  {[type]} res [description]
 * @return {[type]}     [description]
 */
exports.fetchAllUsers = function(req, res){
  var query, options, criteria = {};

  options = _utils.parsedQueryOptions(req.query);
  if(req.query){
    if(req.query.name) criteria = {name: {$regex: formatStringSearchVariable(req.query.name)}};
    if(req.query.first_name) criteria.first_name = {$regex: formatStringSearchVariable(req.query.first_name)};
    if(req.query.last_name) criteria.last_name = {$regex: formatStringSearchVariable(req.query.last_name)};
    if(req.query.feature_identifier){
      criteria.create_date = ( req.query.direction &&
                                  req.query.direction == 'older') ?
                                    {$lt: req.query.feature_identifier} :
                                    {$gt: req.query.feature_identifier};
    }
  }

  query = _utils.buildQueryObject(User, criteria, options);
  query.select('name first_name last_name profile_photo_url').exec(function(err, users){
    if(err || !users){
      _utils.prismResponse(res,null,false,PrismError.invalidUserRequest);
    }else{
      _utils.prismResponse(res,users,true);
    }
  });
};

/**
 * Fetchs Prism user object by identifier
 *
 * @param  {HTTPRequest} req The request object
 * @param  {HTTPResponse} res The response object
 * @return {User} Returns the valid user object to the response object
 */
exports.fetchUser = function(req, res){
  if(req.params.id){
    //User.findOne({_id: req.params.id}, function(error, result){
    var criteria = {_id: req.params.id};
    new Twine('User', criteria, req, null, function(error, result){
      if(error){
        _logger.log('Error', 'Error retrieving user by id: ' + req.params.id);
        _utils.prismResponse(res, null, false, PrismError.invalidUserRequest);

      }else{
        _utils.prismResponse(res, result, true);
      }
    });
  }else{
    _utils.prismResponse(res, null, false, PrismError.invalidUserRequest);
  }
};

/**
 * Updates available User object fields
 *
 * @param  {HTTPRequest} req The request object
 * @param  {HTTPResponse} res The response object
 * @return {Post} Returns A Post Object array containing ..
 */
exports.updateUser = function(req, res){
  if(req.params.id && Object.keys(req.body).length > 0){
    User.findOne({_id: req.params.id}, function(err, user){
      var error = {
        status_code: 400,
        error_info: {
          error: 'unable_to_update_user',
          error_description: 'An error occured while trying to update the user, please try again.'
        }
      };

      if(err || !user){
        _utils.prismResponse(res, null, false, PrismError.invalidUserRequest);
      }else{
        //check updateable body fields & update them if they exist
        var body = req.body;
        if(typeof(body.first_name) !== 'undefined') user.first_name = body.first_name;
        if(typeof(body.last_name) !== 'undefined') user.last_name = body.last_name;
        if(typeof(body.info) !== 'undefined') user.info = body.info;
        if(typeof(body.website) !== 'undefined') user.website = body.website;
        if(typeof(body.ethnicity) !== 'undefined') user.ethnicity = body.ethnicity;
        if(typeof(body.affiliations) !== 'undefined') user.affliations = body.affliations;
        if(typeof(body.religion) !== 'undefined') user.religion = body.religion;
        if(typeof(body.gender) !== 'undefined') user.gender = body.gender;
        if(typeof(body.zip_postal) !== 'undefined') user.zip_postal = body.zip_postal;
        if(typeof(body.birthday) !== 'undefined') user.birthday = body.birthday;
        if(typeof(body.profile_photo_url) !== 'undefined') user.profile_photo_url = body.profile_photo_url;
        if(typeof(body.cover_photo_url) !== 'undefined') user.cover_photo_url = body.cover_photo_url;
        if(typeof(body.instagram_token) !== 'undefined') user.instagram_token = body.instagram_token;
        if(typeof(body.instagram_min_id) !== 'undefined') user.instagram_min_id = body.instagram_min_id;
        if(typeof(body.twitter_token) !== 'undefined') user.twitter_token = body.twitter_token;
        if(typeof(body.twitter_min_id) !== 'undefined') user.twitter_min_id = body.twitter_min_id;
        if(typeof(body.mascot) !== 'undefined') user.mascot = body.mascot;
        if(typeof(body.date_founded) !== 'undefined') user.date_founded = body.date_founded;
        if(typeof(body.enrollment) !== 'undefined') user.enrollment = body.enrollment;

        user.save(function(err, saved){
          if(err || !saved){
            _utils.prismResponse(res, null, false, error);

          }else{
            _utils.prismResponse(res, saved.format('basic'), true);
          }
        });
      }
    });
  }else{
    _utils.prismResponse(res, null, false, PrismError.invalidRequest);
  }
};

/**
 * [fetchUserNewsFeed description]
 * @param  {HTTPRequest} req The request object
 * @param  {HTTPResponse} res The response object
 * @return {Post} Returns A Post Object array containing ..
 */
exports.fetchUserNewsFeed = function(req, res){
  if(req.params.id){
    User.findOne({_id: req.params.id}, function(err, user){

      if(err || !user){
        _utils.prismResponse(res,null,false,PrismError.invalidUserRequest);

      }else{
        //fetch all posts that are public & the user is following
        var following_array = [];
        var trusts_array = [];
        for(var i = 0; i < user.following.length; i++){
          following_array.push(user.following[i]._id);
        }

        Trust.find({status: 'accepted', $or : [{to:req.params.id},{from:req.params.id}]}, function(err, trusts){
          if(err){
            _logger.log('error', 'an error was returned while trying to fetch trusts for feed for user: '+req.params.id);
            _utils.prismResponse(res, null, false, PrismError.serverError);

          }else{
            if(_.has(trusts, 'length')){
              for(var t=0; t < trusts.length; t++){
                var trust = trusts[t].toObject();
                var to = trust.to.toString();
                var from = trust.from.toString();
                var item;
                if(to === req.params.id){
                  item = from;
                }else{
                  item = to;
                }
                trusts_array.push(item);
              }
            }
            //user should see its own posts, so add the user to the following_array
            //which is used in the search criteria

            //TODO: ensure trust is accepted
            var posts_array = [];

            var criteria = {$or: [  {scope: 'public', status: 'active', creator: {$in:following_array}},
                                    {scope: {$in:['trust', 'public']}, status: 'active', creator: {$in:trusts_array}},
                                    {creator: user._id, status: 'active'}]};
            new Twine('Post', criteria, req, null, function(err, result){
              if(err) _utils.prismResponse(res, null, false, PrismError.serverError);
              if(result){
                _utils.prismResponse(res, result, true);
              }else{
                var error = {
                  status_code: 400,
                    error_info: {
                      error: 'user_has_no_news_feed_content',
                      error_description: 'The requested user is not following anyone.'+
                      ' There is no content to display'
                    }
                  };
                _utils.prismResponse(res,null,false,error);
              }
            });
          }
        });
      }
    });
  }else{
    _utils.prismResponse(res,null,false,PrismError.invalidRequest);
  }
};


//TODO: move to posts route class
/**
 * [createUserPost description]
 * @param  {[type]} req [description]
 * @param  {[type]} res [description]
 * @return {[type]}     [description]
 */
exports.createUserPost = function(req, res){
  if(req.params.id){
    if(req.body.text || req.body.file_path){
      var post = new Post({
        category: req.body.category,
        creator: req.body.creator
      });

      if(req.body.location_longitude != 'undefinded' && req.body.location_latitude != 'undefined'){
        post.location_longitude = req.body.location_longitude;
        post.location_latitude = req.body.location_latitude;
        post.location_name = req.body.location_name;
      }

      if(req.body.file_path && req.body.file_path != 'undefined') post.file_path = req.body.file_path;
      if(req.body.text && req.body.text != 'undefined') post.text = req.body.text;
      if(req.body.scope != 'undefined') post.scope = req.body.scope;
      if(req.body.external_provider !== 'undefined') post.external_provider = req.body.external_provider;
      if(req.body.external_link !== 'undefined') post.external_link = req.body.external_link;
      if(req.body.hash_tags){
        post.hash_tags = req.body.hash_tags;
        post.hash_tags_count = req.body.hash_tags.length;
      }

      if(typeof req.body.origin_post_id !== 'undefined'){
        post.origin_post_id = req.body.origin_post_id;
        post.is_repost = true;
      }

      User.findOne({_id: req.params.id}, function(error, user){
        if(error){
          console.log('Error retrieving user by id: ' + req.params.id);
          _utils.prismResponse(res, null, false, PrismError.invalidUserRequest);

        }else{
          post.type = user.type;
          post.target_id = user._id;
          post.save(function(error, user_post){
            if(error){
              _logger.log('error', 'Error trying to create/save a new post',
                          { post_object: post,
                            request_body: req.body,
                            user_object: user,
                            post_error: error });
              _utils.prismResponse(res, null, false, PrismError.invalidUserRequest);

            }else{
              Post.findOne({_id: user_post._id})
              .populate('creator', 'first_name last_name profile_photo_url')
              .exec(function(err, usr){

                //update post count on creator object
                User.findOne({_id: req.body.creator}, function(err, c_user){
                  if(err){
                    console.log(err);
                    _utils.prismResponse(res, null, false, PrismError.serverError);
                  }else{
                    c_user.posts_count = c_user.posts_count+1;
                    c_user.save(function(err, updated_count){
                      if(err){
                        console.log(err);
                        _utils.prismResponse(res, null, false, PrismError.serverError);
                      }else{
                        var activity;
                        if(usr.is_repost){
                          usr.fetchRepostShortUser(usr.origin_post_id, function(err, org_user){
                            usr = usr.toObject();
                            usr.origin_post_creator = org_user;
                            _utils.prismResponse(res, usr, true);
                          });

                        }else{
                          _utils.prismResponse(res, usr, true);
                        }
                      }
                    });
                  }
                });
              });
            }
          });
        }
      });
    }else{
      _logger.error('Invalid Request for create posts.' +
                    ' Missing either text or file_path ', {request_body: req.body});
      _utils.prismResponse(res, null, false, PrismError.invalidRequest,
                                              PrismError.invalidRequest.status_code);
    }

  }else{
    _logger.error('Invalid request for create posts. '+
                  ' Missing user id', {request_params: req.params});
    _utils.prismResponse(res, null, false, PrismError.invalidUserRequest,
                                            PrismError.invalidUserRequest.status_code);
  }
};


//TODO: move to posts route class
/**
 * Fetchs Prism Users posts
 *
 * @param  {HTTPRequest} req The request object
 * @param  {HTTPResponse} res The response object
 * @return {Post} Returns the User.posts subdocument array
 */
exports.fetchUserPosts = function(req, res){
  var fetch_criteria = {};
  var fetch_query, fetch_options;

  _logger.info('fetch users posts params: ', req.params);

  if(req.params.id){
    if(req.query){
      fetch_options = _utils.parsedQueryOptions(req.query);
      if(req.query.feature_identifier){
        if(req.query.direction && req.query.direction == 'older'){
          fetch_criteria = {
            target_id: req.params.id,
            create_date: { $lt: req.query.feature_identifier},
            status: 'active'
          };
        }else{
          fetch_criteria = {
            target_id: req.params.id,
            create_date: { $gt: req.query.feature_identifier},
            status: 'active'
          };
        }

        fetch_query = _utils.buildQueryObject(Post, fetch_criteria, fetch_options);
      }else{
        fetch_criteria = {target_id: req.params.id, status: 'active'};
        fetch_query = _utils.buildQueryObject(Post, fetch_criteria, fetch_options);
      }

    }else{
      fetch_criteria = {_id: req.params.id};
      fetch_query = _utils.buildQueryObject(Post, fetch_criteria);
    }

    var fetch_populate = ['creator', 'first_name last_name profile_photo_url'];
    fetch_query.populate(fetch_populate).exec(function(error, user_posts){

      if(error){
        _logger.error('error', 'Error retrieving by user_id: ', req.params.id);
        _utils.prismResponse(res, null, false, PrismError.invalidUserRequest);

      }else{

          _utils.prismResponse(res, user_posts, true);
      }
    });
  }else{
    _utils.prismResponse(res, null, false, PrismError.invalidUserRequest,
                                            PrismError.invalidUserRequest.status_code);
  }
};

/**
 * Validates the required User properties are present
 * for registration
 *
 * @param  {HTTPRequest} req The request object
 * @return {Boolean}
 */
var isValidRegisterRequest = function(req){
  if( typeof(req.body.first_name) == 'undefined'  ||
      typeof(req.body.last_name) == 'undefined'   ||
      typeof(req.body.gender) == 'undefined' ||
      typeof(req.body.zip_postal) == 'undefined' ||
      typeof(req.body.city) == 'undefined' ||
      typeof(req.body.state) == 'undefined' ||
      typeof(req.body.birthday) == 'undefined' ){

    return false;

  }else{
    if(!isSocialProvider(req.body)){
      if(typeof(req.body.email) == 'undefined'){
        return false;
      }
    }
    return true;
  }
};

/**
 * [isValidSocialRegisterRequest description]
 * @param  {[type]}  req
 * @return {Boolean}
 */
var isValidSocialRegisterRequest = function(req){
  if(isValidRegisterRequest(req)){
    if( typeof(req.body.provider) == 'undefined' ||
        typeof(req.body.provider_token) == 'undefined'  ){
      return false;
    }else{
      if(req.body.provider == 'twitter'){
        if(typeof(req.body.provider_token_secret) == 'undefined') return false;
      }

      return true;
    }
  }else{
    return false;
  }
};

/**
 * Takes passed plain text password & hashes it to
 * validate it equals the stored hash password
 *
 * @param  {User} user The located User object
 * @param  {String} password_to_validate The password to validate
 * @return {Boolean}
 */
var hashAndValidatePassword = function(user, password_to_validate){
  //create user hash
  if(user){
    var hash_salt        = user.createUserSalt();
    var hashed_password  = _utils.prismEncrypt(password_to_validate, hash_salt);
    if(hashed_password == user.password) return true;
  }
  return false;
};

/**
 * [handleSocialProvider description]
 * @param  {[type]}   body
 * @param  {Function} callback
 * @return {[type]}
 */
var handleSocialProviderLogin = function(body, callback){
  switch(body.provider){
    case 'facebook':
      var fb = new Facebook(body.provider_token);
      fb.authorizeUser(function(error, response){
        if(error){
          // console.log('authorize on login did error: ' + error);
          callback(error, false);
        }else if(response){

          var user = User.findOne({provider_id: response.body.id});
          user.select(User.selectFields('basic').join(" "));
          user.exec(function(error, response){
            if(error){
              callback(PrismError.serverError, false);
            }else if(response && response._id){
              callback(false, response);
            }else{
              callback(PrismError.invalidSocialUser, false);
            }
          });
        }else{
          callback(PrismError.serverError, false);
        }
      });
      break;
    case 'twitter':
      var tw = new Twitter(body.provider_token, body.provider_token_secret);
      tw.authorizeUser(function(error, result){
        if(error){
          _logger.error('Error returned attempting to authorize twitter user: ',
                    error);
          callback(error, false);

        }else if(result){
          _logger.info('Succesfuly returned object from authorizing twitter user: ', result);
          _logger.log(result);

          var user = User.findOne({provider_id: result.id.toString()});
          user.select(User.selectFields('basic').join(" "));
          user.exec(function(error, response){
            if(error){
              _logger.error('Error returned trying to find twitter user in prism.'+
                            ' Users does not exist.', {error: error, twitter_user: result});
              callback(PrismError.invalidSocialUser, false);

            }else if(response && response._id){
              _logger.info('Found twitter user to validate login', {user: response});
              callback(false, response);

            }else{
              _logger.warn('Did not find an error or result in fetching twitter user');
              callback(PrismError.invalidSocialUser, false);
            }
          });

        }else{
          _logger.error('A server error occured. No error or'+
                       ' result was retured from authorizing a twitter user');
          callback(PrismError.serverError, false);
        }
      });
      break;
    case 'google':
      var gplus = new Google(body.provider_token);
      gplus.authorizeUser(function(error, result){
        if(error){
          callback(error, false);

        }else if(typeof result.id !== 'undefined'){
          //lookup user by provider_id to ensure exists, otherwise registration is required
          var user = User.findOne({provider_id: result.id.toString()});
          user.select(User.selectFields('basic').join(' '));
          user.exec(function(error, response){
            if(error || !response){
              _logger.log('error', 'Unable to find googleplus user or server error was thrown',
                          {error: error, response: response});
              callback(PrismError.invalideSocialUser, false);
            }

            _logger.log('info', 'Found Google Plus user to validate login', {user: response});
            callback(false, response);
          });

        }else{
          _logger.log('error', 'Unhandled event caused by no error or result returning',
                      {result: result, error: error});
          callback(PrismError.serverError, false);
        }
      });
      break;
    default:
      _logger.log('A unsupported provider type was passed to user registration ',
                  {provider: body.provider});

      callback(PrismError.unsupportedProviderType(body.provider), false);
      break;
  }
};

/**
 * [handleSocialProviderRegistration description]
 * @param  {[type]}   body     [description]
 * @param  {Function} callback [description]
 * @return {[type]}            [description]
 */
var handleSocialProviderRegistration = function(body, callback){
  switch(body.provider){
    case 'facebook':
      var fb = new Facebook(body.provider_token);
      fb.authorizeUser(function(error, response){
        if(error){
          _logger.log('fb authorize error in handleRegistration :',
                      {error: error});

          callback(error, false);
        }else{
          _logger.log('succesful auth & callback with fb authorizeUSer in handleRegistration ',
                      {fb_response_body: response.body});

          callback(false, response.body);
        }
      });
      break;
    case 'twitter':
      var tw = new Twitter(body.provider_token, body.provider_token_secret);
      tw.authorizeUser(function(error, result){
        if(error){
          _logger.error('tw authorize error in handleRegistration: ',
                      {error: error});
          callback(error, false);
        }else{
          _logger.info('succesful auth & response with tw authorizeUser in hanldeRegistration ',
                        {tw_response_body: result});
          callback(false, result);
        }
      });
      break;
    case 'google':
      var gplus = new Google(body.provider_token);
      gplus.authorizeUser(function(error, result){
        if(error || !result){
          _logger.error('unable to authorize googleplus user in handleRegistration: ',
                       {error: error, result: result});
          callback(error, false);

        }else if(result.id){
          _logger.log('info','successful auth & response for googkeplus authroizeUser in reg');
          callback(false, result);

        }else{
          _logger.log('error', 'Unhandled situation with no error and no response from googleplus reg');
        }
      });
      break;
    default:
      //there is no default. therefore the requested provider authorization is
      //not currently supported. Log & reutrn error
      _logger.log('A unsupported provider type was passed to user registration ',
                  {provider: body.provider});

      callback(PrismError.unsupportedProviderType(body.provider), false);
      break;
  }
};

/**
 * Validates the login request body has required properties to
 * process authenticating the user. This can be in the form of
 * traditional authentication {username:pass} OR social authentication
 * which requries {provider, provider_id, & provider_token}
 *
 * @param  {Object}  body The request body object
 * @return {Boolean}
 */
var isValidLoginRequest = function(body){
  // console.log("is valid login request: (body) " + JSON.stringify(body));
  if(body.email && body.password){
    return true;
  }else{
    if(isSocialProvider(body)){
      return true;
    }
    return false;
  }
};

/**
 * Determines if the request body is/has social provider attributes
 *
 * @param  {Object}  body The request body object
 * @return {Boolean}
 */
var isSocialProvider = function(body){
  if(body.provider && body.provider_token){
    console.log('isSocialProvider -- body:' + JSON.stringify(body));
    return true;
  }
  return false;
};

/**
 * Search User
 */
exports.search = function(req, res){
  //create invalid search error;
  var error = {
    status_code: 400,
    error_info: {
      error: 'invalid_search_request',
      error_description: 'A search key & object value must be included in the X-Arguments httpd header'
    }
  };
  if(req.headers['x-arguments']){
    var args = new Buffer(req.headers['x-arguments'], 'base64').toString('utf8');
    args = JSON.parse(args);
    if(!args.search) _utils.prismResponse(res, null, false, error);

    var search_key = Object.keys(args.search)[0];
    if(!search_key) _utils.prismResponse(res, null, false, error);
    var criteria = {};
    criteria[search_key] = {$regex: formatStringSearchVariable(args.search[search_key])};
    new Twine('User', criteria, req, null, function(err, response){
      if(err) _utils.prismResponse(res, null, false, PrismError.serverError);
      _utils.prismResponse(res, response, true);
    });
  }else{
    _utils.prismResponse(res, null, false, error);
  }
};
