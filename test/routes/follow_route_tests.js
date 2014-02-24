/**
 * Follow Route Unit Tests
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
    User          = require(_prism_home + 'models/user').User;

describe('Follow Route Unit Tests', function(done){
  var mark, edwardo, cameron, erica, sean, maryolin;
  var follower = null;
  var followee = null;
  var follow_result = { followee: null, follower: null};
  var request_result = null;
  var test_user = null;
  var test_token = null;
  var test_client = null;
  var test_code = null;

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
  before(function(done){
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
            if(i !== 0) executeFollowRequest(test_user, social_network_followers[i], null);
            // if(i == social_network_followers.length) done();
          }
          done();
        });
      });
    });
  });

  describe('Testing following a User', function(done){
    before(function(done){
      _t_helpers.createTestUser(function(user_follower){
        follower = user_follower;
        _t_helpers.createTestUser(function(user_followee){
          followee = user_followee;
          executeFollowRequest(user_follower, user_followee, function(err, result){
            if(err) throw err;
            request_result = result;
            User.findOne({_id: user_follower}, function(err,resl){
              if(err) throw err;
              follow_result.follower = resl;
              User.findOne({_id: user_followee}, function(err, reslt){
                if(err) throw err;
                follow_result.followee = reslt;
                done();
              });
            });
            // done();
          });
        });
      });
    });

    it('should return a successful response with no data', function(done){
      console.log(request_result);
      _expect(request_result.metadata.success).to.equal(true);
      done();
    });
    it('should follow a user and update the followees followers property', function(done){
      _expect(follow_result.followee).to.have.property('followers');
      _expect(follow_result.followee).to.have.property('following');
      _expect(follow_result.followee).to.have.property('following_count');
      _expect(follow_result.followee).to.have.property('followers_count');

      _expect(follow_result.followee.followers[0]._id).to.equal(follower._id.toString());
      _expect(follow_result.followee.followers_count).to.equal(1);
      done();
    });
    it('should follow a user and update the followers following & count properties', function(done){
      _expect(follow_result.follower).to.have.property('followers');
      _expect(follow_result.follower).to.have.property('following');
      _expect(follow_result.follower).to.have.property('following_count');
      _expect(follow_result.follower).to.have.property('followers_count');

      _expect(follow_result.follower.following[0]._id).to.equal(followee._id.toString());
      _expect(follow_result.follower.following_count).to.equal(1);
      done();
    });
  });

  describe('Testing fetching a users followers', function(done){

    var follower = null;
    var base_url = 'https://localhost:3000/users/';
    var fetch_result = null;

    after(function(done){
      _t_helpers.destroyTestUser(function(){
        done();
      });
    });

    it('should not allow you to follow someone twice', function(done){
      executeFollowRequest(mark, test_user, function(err, result){
        _expect(result.metadata.success).to.equal(false);
        _expect(result.error.error).to.equal('unable_to_follow_user');
        done();
      })
    });
    it('should return follwers', function(done){
      _request({
        method: "GET",
        strictSSL: false,
        json: true,
        headers: {"Authorization": 'Bearer '+ test_token.access_token},
        url: base_url + test_user._id + '/followers'
      }, function(err, response){
        console.log(JSON.stringify(response.body));
        done();
      });

    });
    it('should return following', function(done){
      _request({
        method: "GET",
        strictSSL: false,
        json: true,
        headers: {"Authorization": 'Bearer '+ test_token.access_token},
        url: base_url + test_user._id + '/following'
      }, function(err, response){
        console.log(JSON.stringify(response.body));
        done();
      });
    });
  });
});
