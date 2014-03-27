/**
 *  Twine Class Unit Tests
 *
 * @author  DJ Hayden <dj.hayden@stablekernel.com>
 */
/*jshint -W087 */
var _thisapp    = require(process.cwd() + '/server.js'),
    _request    = require('request'),
    _mongoose   = require('mongoose'),
    _chai       = require('chai'),
    _expect     = _chai.expect,
    _should     = _chai.should(),
    _assert     = _chai.assert,
    _logger     = require('winston'),
    _helpers    = require(process.env.PRISM_HOME + 'test/test_helpers'),
    Twine       = require(process.env.PRISM_HOME + 'classes/Twine'),
    Post        = require(process.env.PRISM_HOME + 'models/post').Post,
    User        = require(process.env.PRISM_HOME + 'models/user').User,
    Activity    = require(process.env.PRISM_HOME + 'models/activity').Activity,
    Trust       = require(process.env.PRISM_HOME + 'models/user').Trust;

describe('Twine Class Unit Tests', function(done){
  var mark, edwardo, cameron, erica, sean, maryolin;
  var follower = null;
  var followee = null;
  var follow_result = { followee: null, follower: null};
  var request_result = null;
  var test_user = null;
  var test_token = null;
  var test_client = null;
  var test_code = null;
  var test_post1 = null;
  var test_post2 = null;
  var test_post3 = null;
  var test_post4 = null;
  var test_post5 = null;
  var test_post6 = null;
  var test_post7 = null;
  var test_post8 = null;
  var test_comment_id = null;
  var test_comment_id2 = null;

  var executeRequest = function(method, url, body, cb){
    _request({
      method: method,
      strictSSL: false,
      json: true,
      url: 'https://localhost:3000/' + url,
      headers: {"Authorization":"Bearer " + test_token.access_token},
      body: (!body)? {} : body
    }, function(err,result){
      if(cb) cb(err, result.body);
    });
  };

  var setupSocialNetworkUsers = function(cb){
    var users = _helpers.fetchFakeUsersArray();
    User.create(users, function(err, m, e, c, e2, s, m2){
      if(err) throw err;
      mark = m;
      edwardo = e;
      cameron = c;
      erica = e2;
      sean = s;
      maryolin = m2;
      cb();
    });
  };

  var executeFollowRequest = function(u_follower, u_followee, cb){
    _request({
      method: 'POST',
      strictSSL: false,
      json: true,
      url: 'https://localhost:3000/users/'+u_followee._id+'/follow',
      headers: {"Authorization": 'Bearer ' + test_token.access_token},
      body: {creator: u_follower._id}
    }, function(err, result){
      if(cb) cb(err, result.body);
    });
  };

  var executeLikeRequest = function(type, u_creator, post_id, cb){
    _request({
      method: 'POST',
      strictSSL: false,
      json: true,
      url: 'https://localhost:3000/posts/'+post_id+'/'+type,
      headers: {"Authorization" : 'Bearer ' + test_token.access_token},
      body: {creator: u_creator}
    }, function(err, res){
      if(cb) cb(err, res.body);
    });
  };

  var executeCommentLikeRequest = function(type, u_creator, post_id, comment_id, cb){
    _request({
      method: "POST",
      strictSSL: false,
      json:true,
      url: 'https://localhost:3000/posts/'+post_id+'/comments/'+comment_id+'/'+type,
      headers: {'Authorization':'Bearer ' + test_token.access_token},
      body: {creator: u_creator}
    }, function(err, response){
      if(cb) cb(err, response.body);
    });
  };

  var executeAddCommentRequest = function(u_creator, post_id, cb){
    _request({
      method: 'POST',
      strictSSL: false,
      json: true,
      url: 'https://localhost:3000/posts/' + post_id + '/comments',
      headers: {"Authorization" : "Bearer " + test_token.access_token},
      body: {creator: u_creator, text: 'test commenting on this post'}
    }, function(err, result){
      if(cb) cb(err, result.body);
    });
  };

  var executeDeleteCommentRequest = function(post_id, comment_id, cb){
    _request({
      method: 'DELETE',
      strictSSL: false,
      json: true,
      url: 'https://localhost:3000/posts/'+post_id+'/comments/'+comment_id,
      headers: {'Authorization' : 'Bearer '+ test_token.access_token}
    }, function(err, result){
      if(cb) cb(err, result.body);
    });
  };

  var executeDeletePostRequest = function(post_id, creator, cb){
    executeRequest('DELETE', 'posts/'+post_id, {creator: creator}, function(err, res){
      if(cb) cb(err, res);
    });
  };

  var executeFlagPostRequest = function(post_id, reporter_id, cb){
    executeRequest('POST', 'posts/'+post_id+'/flag', {reporter: reporter_id}, function(err, res){
      if(cb) cb(err, res);
    });
  };

  var executeUpdatePostRequest = function(post_id, updated_post, cb){
    executeRequest('PUT', 'posts/'+post_id, updated_post, function(err, res){
      if(cb) cb(err,res);
    });
  };

  before(function(done){
    _helpers.destroyTestUser(function(){
      _helpers.createTestUser(function(testuser){
        test_user = testuser;

        _helpers.createTestToken(function(token, code, client){
          test_token = token;
          test_code = code;
          test_client = client;
          setupSocialNetworkUsers(function(c){
            var social_network_followers = [  mark,
                                              edwardo,
                                              cameron,
                                              erica,
                                              sean,
                                              maryolin ];

            for(var i = 0; i < social_network_followers.length; i++){

              executeFollowRequest( social_network_followers[i],
                                    test_user,
                                    null);

              if(i !== 3) executeFollowRequest( test_user,
                                                social_network_followers[i],
                                                null );
            }
            //setup fake posts
            var posts = _helpers.fetchFakePostsArray(mark, test_user);
            Post.create(posts, function(err, test1, test2, test3, test4, test5, test6, test7, test8){
              test_post1 = test1;
              test_post2 = test2;
              test_post3 = test3;
              test_post4 = test4;
              test_post5 = test5;
              test_post6 = test6;
              test_post7 = test7;
              test_post8 = test8;
              // executeAddCommentRequest()
              done();
            });
          });
        });
      });
    });
  });

  after(function(done){
    _helpers.destroyTestUser(function(){
      _helpers.destroyTestPost(function(){
        done();
      });
    });
  });

  describe('testing somethin else', function(done){
    before(function(done){

      User.findOne({_id: mark._id}, function(err, result){
        result.following = [
          {"_id": sean._id, "create_date": new Date()},
          {"_id": cameron._id, "date": new Date()},
          {"_id": cameron._id, "date": new Date()}
        ];
        result.save(function(){
          done();
        });
      });
    });
    it('request test', function(done){
      var req = {
        method: 'GET',
        url: 'https://localhost:3000/users/'+mark._id/+'following',
        strictSSL: false,
        json: true,
        body: {
          first_name: 'mark',
          status: 0,
          sort: 1,
          sort_by:'create_date',
          resolve: {
            following: {
              format:'short',
              fields: ['create_date', 'test']
            },
            followers: {
              format: 'short'
            }
          },
          contains: {
            following: {
              _id: sean._id.toString()
            }
          }
        }
      };

      new Twine('User', {_id: mark._id.toString()}, req, null, function(err, result){
        console.log(result);
        debugger;
        done();
      });
    });
  });
});





















