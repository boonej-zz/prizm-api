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
  describe('Testing following a User', function(done){
      var follower = null;
      var followee = null;
      var follow_result = { followee: null, follower: null};
      var request_result = null;
      var test_token = null;
      var test_client = null;
      var test_code = null;

      var executeFollowRequest = function(u_follower, u_followee, cb){
        _request({
          method: 'POST',
          strictSSL: false,
          json: true,
          url: 'https://localhost:3000/users/'+u_followee._id+'/follow',
          headers: {"Authorization": 'Bearer ' + test_token.access_token},
          body: {creator: u_follower._id}
        }, function(err, result){
          cb(err, result.body);
        });
      };

      before(function(done){
        _t_helpers.createTestToken(function(token, code, client){
          test_token = token;
          test_code = code;
          test_client = client;

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
      });

      after(function(done){
        _t_helpers.destroyTestUser(function(){
          done();
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
      var base_url = 'https://localhost:3000/users';
      var fetch_result = null;

    });
});
