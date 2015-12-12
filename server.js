/**
 * Prism Mobile REST API Server
 *
 * @author DJ Hayden <dj.hayden@stablekernel.com>
 */
require('newrelic');

process.env.PRISM_HOME = process.cwd() + '/';

var _prism_home     = process.env.PRISM_HOME;
require(_prism_home + 'models/insight');
require(_prism_home + 'models/interest');
require(_prism_home + 'models/organization');
require(_prism_home + 'models/group');
require(_prism_home + 'models/message');
require(_prism_home + 'models/mute');
require(_prism_home + 'models/invite');
require(_prism_home + 'models/survey');

var _express        = require('express'),
    _mongoose       = require('mongoose'),
    _http           = require('http'),
    _fs             = require('fs'),
    _https          = require('https'),
    _auth           = require(_prism_home + 'routes/oauth2/auths'),
    _token          = require(_prism_home + 'routes/oauth2/tokens'),
    _user           = require(_prism_home + 'routes/users'),
    _insight        = require(_prism_home + 'routes/insights'),
    _explore        = require(_prism_home + 'routes/explore'),
    _follow         = require(_prism_home + 'routes/follow'),
    _post           = require(_prism_home + 'routes/posts'),
    _trust          = require(_prism_home + 'routes/trusts'),
    _utils          = require(_prism_home + 'utils'),
    _gateway        = require(_prism_home + 'gateway'),
    _config         = require('config'),
    _e_winston      = require('express-winston'),
    logger          = require(_prism_home + 'logs.js'),
    PrismError      = require(_prism_home + 'error'),
    _winston        = require('winston'),
    ActivityListener = require(_prism_home + 'classes/ActivityListener'),
    _activity       = require(_prism_home + 'routes/activities'),
    _interest       = require(_prism_home + 'routes/interests'),
    _organization   = require(_prism_home + 'routes/organizations'),
    _message        = require(_prism_home + 'routes/messages'),
    _survey         = require(_prism_home + 'routes/surveys');
var v2 = require(_prism_home + 'v2');
var path = require('path');
    
    new ActivityListener();

var _app            = _express();
var _httpserver     = _express();

//general settings */
_app.use(_express.bodyParser());
_app.use(_express.methodOverride());
var basicAuth = _express.basicAuth(function(user, pass){
  return 'higheraltitude' === user && '@pple4life' === pass;
});
_app.use('/docs', basicAuth);
_app.use('/docs', _express.static(path.join(__dirname, 'public/doc')));

/* environment specific settings */
  errorTransports = [
    new _winston.transports.File({
      filename: '/mnt/extended/prism_errors.log',
      json: true,
      colorize: true
    })
  ];
  standardTransports = [
    new _winston.transports.File({
      filename: '/mnt/extended/prism_requests.log',
      json: true,
      colorize: true,
      prettyPrint: true
    })
  ];


/* configure mongo connection */
_mongoose.connect('mongodb://' + _config.mongo.host + '/' + _config.mongo.name);

/* express winston logger before router */
_app.use(_e_winston.logger({
  transports: standardTransports,
  meta: true,
  msg: "HTTP METHOD:{{req.method}}  URL:{{req.url}}  QUERY:{{JSON.stringify(req.query)}}  BODY:{{JSON.stringify(req.body)}}"
}));

_app.use('/v2/', v2);
_app.use(_app.router);

/* express winston errorLogger after router */
_app.use(_e_winston.errorLogger({
  transports: errorTransports
}));

//Set SSL options for HTTPS traffic
var ssl_options = {
  key:                  _fs.readFileSync(_prism_home + '/config/ssl/PrizmApp.key'),
  cert:                 _fs.readFileSync(_prism_home + '/config/ssl/star_prizmapp_com.crt'),
  ca:                   _fs.readFileSync(_prism_home + '/config/ssl/DigiCertCA.crt'),
  requestCert:        false,
//  requestCert:       true,
  rejectUnauthorized:   false
};

/* Force all http traffic to https */
_httpserver.set('port', process.env.PORT || 80);
_httpserver.get("*", function(req, res, next){
  res.redirect("https://127.0.0.1:3000" + req.path);
});
process.on('uncaughtException', function (err) {
    console.log(err);
});

/********************** API ROUTES ************************/
/* Root Endpoint */
_app.get('/', function(req,res){ res.send('Welcome to the Prism API'); });

/* Authentication Code Endpoint */
_app.get('/oauth2/authorize', _auth);



/* Default Authorization Code RedirectUri Callback Endpoint - FOR PRISM MOBILE USE ONLY */
_app.get('/callback', function(req, res){
  var array = [{authorization_code: req.query.code}];
  _utils.prismResponse( res, array, true);
});

/* Token Request Endpoint */
_app.post('/oauth2/token', _token);

/* User Login/Authentication */
_app.post('/oauth2/login', _gateway, _user.login);

/* Fetch All Users */
_app.get('/users', _gateway, _user.fetchAllUsers);

/* Create/Register User Route */
_app.post('/users', _gateway, _user.register);

/* Fetch User */
_app.get('/users/:id', _gateway, _user.fetchUser);

/* Update User */
_app.put('/users/:id', _gateway, _user.updateUser);

/* Fetch Users Posts */
_app.get('/users/:id/posts', _gateway, _post.fetchUserPosts);
// _app.get('/users/:id/posts', _gateway, _user.fetchUserPosts);

/* Fetch Liked Posts */
_app.get('/users/:id/likes', _gateway, _post.fetchLikedPosts);

/* Add Post to User */
_app.post('/users/:id/posts', _gateway, _user.createUserPost);

/* Update parent consent */
_app.put('/users/:uid/consent', _gateway, _user.parentConsent);

/* Get user surveys */
_app.get('/users/:uid/surveys', _gateway, _survey.fetchUserSurveys);

/* Get completed user surveys */
_app.get('/users/:uid/surveys/completed', _gateway, _survey.getUserSurveys);

_app.get('/organizations/:oid/surveys/latest', _gateway, _survey.getLatestSurvey);

/* Fetch Post by Identifier */
_app.get('/posts/:id', _gateway, _post.fetchPostById);

/* Add Comment to Post */
_app.post('/posts/:id/comments', _gateway, _post.createPostComment);

/* Flag a Post An Inappropriate */
_app.post('/posts/:id/flag', _gateway, _post.flagPost);

/* Delete Post   */
_app.delete('/posts/:id', _gateway, _post.removePost);

/* Update a Post */
_app.put('/posts/:id', _gateway, _post.updatePost);

/* Delete Comment From Post */
_app.delete('/posts/:id/comments/:comment_id', _gateway, _post.removePostComment);

/* Get A Post Comments */
_app.get('/posts/:id/comments', _gateway, _post.fetchPostComments);

/* Like A Post */
_app.post('/posts/:id/like', _gateway, _post.likePost);

/* Fetch Post Likes */
_app.get('/posts/:id/likes', _gateway, _post.fetchPostLikes);

/* Fetch Comment Likes */
_app.get('/posts/:id/comments/:comment_id/likes', _gateway, _post.fetchCommentLikes);

/* Unlike A Post */
_app.post('/posts/:id/unlike', _gateway, _post.unlikePost);

/* Fetch A Posts Like by Request Identifier */
_app.get('/posts/:id/like/:like_id', _gateway, _post.fetchPostAndLikeById);

/* Like A Comment */
_app.post('/posts/:id/comments/:comment_id/like', _gateway, _post.likeComment);

/* Unlike A Comment */
_app.post('/posts/:id/comments/:comment_id/unlike', _gateway, _post.unlikeComment);

/* Fetch A Comment */
_app.get('/posts/:id/comments/:comment_id', _gateway, _post.fetchComment);

/* Post User Interests */
_app.post('/users/:id/interests', _gateway, _user.addInterests);
/* Fetch Users followers */
_app.get('/users/:id/followers', _gateway, _follow.fetchFollowers);

/* Fetch Users following */
_app.get('/users/:id/following', _gateway, _follow.fetchFollowing);

/* Fetch Users News Feed */
_app.get('/users/:id/feed', _gateway, _user.fetchUserNewsFeed);

/* Delete User (set inactive) */
_app.delete('/users/:id', _gateway, _user.deleteUser);

/* Create Trust */
_app.post('/users/:id/trusts', _gateway, _trust.createTrust);

/* Fetch a Trust */
_app.get('/trusts/:id', _gateway, _trust.fetchTrustById);

/* Update Trust */
_app.put('/trusts/:id', _gateway, _trust.updateTrust);

/* Fetch Users Trusts */
_app.get('/users/:id/trusts', _gateway, _trust.fetchTrusts);

/* Review User */
_app.get('/users/:id/review/:review', _user.review);

/* Search for a specific User */
_app.get('/search/users', _gateway, _user.search);

/* Delete User Trust */
_app.delete('/users/:id/trusts/:trust_id', _gateway, _trust.deleteTrust);

/* Validate 2 users are in a trust */
_app.get('/exists/:id/trusts/:user_id', _gateway, _trust.exists);

/* Fetch is User Following By Following Identifier */
_app.get( '/users/:id/following/:following_id',
          _gateway,
          _follow.fetchIsFollowingById );

/* Fetch is User Follower By Follower Identifier */
_app.get( '/users/:id/followers/:follower_id',
          _gateway,
          _follow.fetchIsFollowersById );

/* Follow a User */
_app.post('/users/:id/follow', _gateway, _follow.follow);

/* Unfollow a User */
_app.post('/users/:id/unfollow', _gateway, _follow.unfollow);

/* Fetch Users Activities */
_app.get('/users/:id/activites', _gateway, _activity.fetchUserActivity);

_app.get('/users/:id/stats/category', _gateway, _post.fetchCategoryPostCountByWeekAndYear);

_app.get('/users/:id/stats/hashtags', _gateway, _post.fetchHashTagsForCategory);

/* Reset User Password */
_app.post('/users/:email/passwordreset', _gateway, _user.resetPassword);

/* Reset User Password */
_app.post('/users/:email/passwordchange', _gateway, _user.changePassword);

/* Explore Route */
_app.get('/explore', _gateway, _explore.explore);

/* Search Hash Tags */
_app.get('/search/hashtags/:hash', _gateway, _explore.search);

/* Search For Users to send trust invites to */
_app.get('/search/:id/invite/:name', _gateway, _trust.searchForUsersNotInTrust);

/* Search User in Trusts */
_app.get('/search/:id/trusts/:name', _gateway, _trust.searchForUsersInTrust);

/* Register/Unregister Device for a User */
_app.post('/users/:id/devices', _gateway, _user.registerDevice);

_app.post('/users/:uid/push_enabled', _gateway, _user.setUserPushEnabled); 

/* Unregister devices from push notifications */
_app.post('/devices/:id', _user.unregisterDevice);

_app.get('/files/:name', function(req, res){
  var fileName = req.params.name;
  res.sendfile(__dirname + '/public/' + fileName, function(err){
    res.send(404);
  });
});

_app.post('/insights', _gateway, _insight.createInsight);
_app.get('/insights', _gateway, _insight.fetchInsights);
_app.post('/insights/:id', _gateway, _insight.sendInsight);
_app.get('/users/:id/insights', _gateway, _insight.fetchUserInsights);
_app.post('/insights/:id/like', _gateway, _insight.likeInsight);
_app.post('/insights/:id/dislike', _gateway, _insight.dislikeInsight);

_app.post('/interests', _gateway, _interest.createInterest);
_app.get('/interests', _gateway, _interest.fetchInterests);
_app.get('/organizations/:code', _gateway, _organization.searchOrganizations);
_app.get('/users/:id/organizations', _gateway, _message.fetchOrgs);
_app.get('/users/:user_id/organizations/:org_id/groups', _gateway, _message.fetchGroups);
_app.get('/users/:id/suggestions', _gateway, _follow.fetchSuggestions);
_app.get('/organizations/:org_id/groups/:group_name/messages', _gateway,  _message.fetchMessages);
_app.put('/organizations/:org_id/groups/:group_name/messages/:message_id', _gateway,  _message.updateMessage);
_app.post('/organizations/:org_id/groups/:group/messages', _gateway, _message.createMessage);
_app.post('/organizations/:org_id/users/:uid/messages', _gateway, _message.createDirectMessage);
_app.get('/organizations/:org_id/users/:uid/messages', _gateway, _message.getDirectMessages);
_app.delete('/organizations/:org_id/groups/:group/messages/:message_id', _gateway, _message.deleteMessage);
_app.get('/organizations/:org_id/members', _gateway, _message.fetchGroupMembers);
_app.get('/users/:uid/messages/digest', _gateway, _message.fetchDirectDigest);
_app.post('/organizations/:org_id/groups', _gateway, _message.createGroup);
_app.delete('/organizations/:org_id/groups/:group_id', _gateway, _message.deleteGroup);
_app.put('/organizations/:org_id/groups/:group_id', _gateway, _message.updateGroup);
_app.put('/organizations/:org_id/groups/:group_id/members', _gateway, _message.updateGroupMembers);
_app.delete('/organizations/:org_id/groups/:group_id/members/:user_id', _gateway, _message.deleteUserFromGroup);
_app.put('/organizations/:org_id', _gateway, _message.updateOrganization);

_app.post('/surveys/:sid/questions/:qid', _gateway, _survey.postAnswer);
_app.post('/surveys/:sid/finalize', _gateway, _survey.finalizeSurvey);
_app.get('/organizations/:oid/surveys/leaderboard', _gateway, _survey.getLeaderboard);


/* HACK Find User by instagram_id */
_app.get('/instagram/:id', _gateway, function(req, res){
  if(req.params.id){
    User.findOne({instagram_id: req.params.id}, function(err, result){
      if(err) _utils.prismResponse(res, null, false, PrismError.serverError);
      if(!result) _utils.prismResponse(res, null, false, PrismError.invalidUserRequest);
      _utils.prismResponse(res, result, true);
    });
  }else{
    _utils.prismResponse(res, null, false, PrismError.invalidRequest);
  }
});

/* Testing Endpoints only */
if(_app.get('env')  == 'test'){
  _app.get('/testresponseformat/:id', function(req, res){
    var array = [{response: 'response1'}, {response: 'response2'}];
    var dict  = {response: 'reponse1'};
    switch(req.param.id){
      case 0:
        _utils.prismResponse(res, array, true);
        break;
    default:
        _utils.prismResponse(res, array, true);
        break;
    }
  });
}

/* create regular http server listening on port 8080 that re-routes back to https */
_http.createServer(_httpserver).listen(8080, function(){
  if(_app.get('env') !== 'test') {
    console.log('HTTP Server listening on port 8080');
  }
});

/* create SSL Server listening on port 3000 */
_https.createServer(ssl_options, _app).listen(_config.env.port, function(){
  if(_app.get('env') !== 'test') {
    console.log("Secure Prism Api server listening on port " + _config.env.port);
  }
});
