/**
 * Posts Route Unit Tests
 *
 * @author DJ Hayden <dj.hayden@stablekernel.com>
 */
var _mongoose     = require('mongoose'),
    _request      = require('request'),
    _chai         = require('chai'),
    _expect       = _chai.expect,
    _should       = _chai.should(),
    _assert       = _chai.assert,
    _thisapp      = require(process.cwd() + '/server'),
    _prism_home   = process.env.PRISM_HOME,
    _auth_model   = require(_prism_home + 'models/auth'),
    _follow       = require(_prism_home + 'routes/follow'),
    _t_helpers    = require(_prism_home + 'test/test_helpers'),
    User          = require(_prism_home + 'models/user').User,
    Post          = require(_prism_home + 'models/post').Post;

describe('Posts Route Unit Tests', function(done){
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

  var setupSocialNetworkUsers = function(cb){
    var users = _t_helpers.fetchFakeUsersArray();
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

  before(function(done){
    _t_helpers.destroyTestUser(function(){
      _t_helpers.createTestUser(function(testuser){
        test_user = testuser;

        _t_helpers.createTestToken(function(token, code, client){
          test_token = token;
          test_code = code;
          test_client = client;
          setupSocialNetworkUsers(function(c){
            var social_network_followers = [mark, edwardo, cameron, erica, sean, maryolin];
            for(var i = 0; i < social_network_followers.length; i++){

              executeFollowRequest(social_network_followers[i], test_user, null);
              if(i !== 3) executeFollowRequest(test_user, social_network_followers[i], null);
              // if(i == social_network_followers.length) done();
            }
            //setup fake posts
            var posts = _t_helpers.fetchFakePostsArray(mark, test_user);
            Post.create(posts, function(err, test1, test2, test3){
              test_post1 = test1;
              test_post2 = test2;
              test_post3 = test3;
              // executeAddCommentRequest()
              done();
            });
          });
        });
      });
    });
  });

  after(function(done){
    _t_helpers.destroyTestUser(function(){
      done();
    });
  });

  describe('Testing Fetching a Post by Id', function(done){
    it('should return the entire post object', function(done){
      done();
    });
  });

  describe('Testing Adding a Comment to a Post', function(done){
    it('should add a comment to a specified post', function(done){
      executeAddCommentRequest(test_user._id, test_post1._id, function(err, body){
        _expect(body.metadata.success).to.equal(true);
        _expect(body.data[0]).to.have.property('comments');
        _expect(body.data[0]).to.have.property('comments_count');
        _expect(body.data[0].comments).to.have.property('_id');
        _expect(body.data[0].comments).to.have.property('creator');
        _expect(body.data[0].comments).to.have.property('text');
        _expect(body.data[0].comments).to.have.property('create_date');
        _expect(body.data[0].comments_count).to.equal(1);
        _expect(body.data[0].comments.creator).to.equal(test_user._id.toString());
        _expect(body.data[0].comments.text).to.equal('test commenting on this post');
        done();
      });
    });
    it('should fetch comments from a specific post', function(done){
      executeAddCommentRequest(edwardo._id, test_post1._id, function(err, body){
        if(err) throw err;
        _request({
          method: 'GET',
          json: true,
          strictSSL: false,
          headers: {"Authorization" : "Bearer " + test_token.access_token},
          url: 'https://localhost:3000/posts/'+test_post1._id+'/comments'
        }, function(err, result){
          console.log(result);
          console.log(err);
          debugger;
          done();
        });
      });
    });
  });

  describe('Testing A Users News Feed', function(done){
    //setup test_user to follow mark
    before(function(done){
      //make the assumption everything is fine
      executeFollowRequest(test_user, mark);
      done();
    });
    it('should fetch a users news feed', function(done){
      _request({
        method: 'GET',
        url: 'https://localhost:3000/users/' + test_user._id+'/feed',
        json: true,
        strictSSL: false,
        headers: {"Authorization":"Bearer "+test_token.access_token}
      },function(err, result){
        _expect(result.body.metadata.success).to.equal(true);
        _expect(result.body.data.length).to.be.above(3);


        done();
      });
    });
  });

  describe('Testing Fetching A Post Like by Post Id && Requestor Id', function(done){
    it('should return the post object with likes & count', function(done){
      executeLikeRequest('like', edwardo._id, test_post3._id, function(err, res){
        _request({
          method: 'GET',
          strictSSL: false,
          json: true,
          headers: {"Authorization" : "Bearer "+ test_token.access_token},
          url: 'https://localhost:3000/posts/'+test_post3._id+'/like/'+edwardo._id
        }, function(err, result){
          _expect(result.body.metadata.success).to.equal(true);
          _expect(result.body.data[0].likes_count).to.be.above(0);
          _expect(result.body.data[0].likes[0]._id).to.equal(edwardo._id.toString());
          done();
        });
      });
    });
  });

  describe('Testing Like a Post', function(done){
    it('should update the posts record after a successful like', function(done){
      executeLikeRequest('like',mark._id, test_post1._id, function(err, res){
        _expect(res.metadata.success).to.equal(true);
        _expect(res.data[0].likes_count).to.equal(1);
        _expect(res.data[0].likes[0]._id).to.equal(mark._id.toString());
        done();
      });
    });
    it('should return an error with invalid post_id', function(done){
      done();
    });
    it('should return an error with no creator identifier in post body', function(done){
      executeLikeRequest('like', null, test_post1._id, function(err, res){
        console.log(res);
        _expect(res.error).to.exist;
        _expect(res.error.error).to.equal('invalid_request');
        _expect(res.data).to.be.empty;
        done();
      });
    });
  });
  describe('Testing Unlike a Post', function(done){
    beforeEach(function(done){
      executeLikeRequest('like', test_user._id, test_post2._id, function(err, res){
        if(err) throw err;
        done();
      });
    });

    it('should remove the like from the posts record on successful unlike', function(done){
      executeLikeRequest('unlike', test_user._id, test_post2._id, function(err, res){
        console.log('unlike result logged');
        _expect(res.metadata.success).to.equal(true);
        _expect(res.data[0].likes_count).to.equal(0);
        _expect(res.data[0].likes).to.be.empty;
        done();
      });
    });
  });
});










