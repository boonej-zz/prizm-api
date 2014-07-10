/**
 * Twine Resolve Tests
 *
 * @author DJ Hayden <dj.hayden@stablekernel.com>
 */

/*jshint -W087 */

var app       = require(process.cwd() + '/server.js'),
    home      = process.env.PRISM_HOME,
    mongoose  = require('mongoose'),
    _         = require('underscore'),
    chai      = require('chai'),
    expect    = chai.expect,
    should    = chai.should(),
    assert    = chai.assert,
    helpers   = require(home + 'test/test_helpers'),
    User      = require(home + 'models/user').User,
    Post      = require(home + 'models/post').Post,
    Trust     = require(home + 'models/trust').Trust,
    Twine     = require(home + 'classes/Twine');

describe('Twine Resolve & Contains Edge Cases', function(done) {
  var mark, edwardo, cameron, erica, sean, maryolin, DJ;
  var p1, p2, p3, p4, p5, p6, p7, p8, p9, p10;

  before(function(done) {
    helpers.fetchFixtureTestUsers(function(users) {
      mark      = users.mark;
      edwardo   = users.edwardo;
      cameron   = users.cameron;
      maryolin  = users.maryolin;
      erica     = users.erica;
      sean      = users.sean;
      DJ        = users.DJ;
      done();
    });
  });

  describe('Simple Resolve & Contains', function(done) {
    var xargs, req, result, error;

    before(function(done) {
      xargs = {
        resolve: {
          followers: {
            format: "short"
          }
        },
        contains: {
          followers: {
            _id: mark._id.toString()
          }
        }
      };
      req = helpers.basicRequestObject(xargs);

      new Twine('User', {_id: DJ._id.toString()}, req, null, function(err, res) {
        error = err;
        result = res;
        done();
      });
    });

    it('should not return an error', function(done) {
      assert.isFalse(error);
      done();
    });

    it('should have returned the specified field in the data array', function(done) {
      assert.property(result.data[0], 'followers');
      done();
    });

    it('should only have 1 objectId in the specified field array', function(done) {
      assert.lengthOf(result.data[0].followers, 1);
      done();
    });

    it('should equal the specified contains value', function(done) {
      var item = result.data[0];
      var followers = item.followers[0];
      assert.equal(followers._id, mark._id.toString());
      done();
    });

    it('should return a resolve object', function(done) {
      assert.property(result, 'resolve');
      done();
    });

    it('should resolve a User object', function(done) {
      var resolve = result.resolve;
      assert.property(resolve, 'User');
      assert.isObject(resolve.User);
      done();
    });

    it('should only return one resolved User object', function(done) {
      assert.lengthOf(_.keys(result.resolve.User), 1);
      done();
    });

    it('should equal the resolved user that was specified in contains', function(done) {
      var resolve_user_key = _.keys(result.resolve.User)[0];
      var contains_user    = mark._id.toString();
      var followers_user   = result.data[0].followers[0]._id;

      assert.equal(resolve_user_key, contains_user);
      assert.equal(resolve_user_key, followers_user);
      done();
    });
  });

  describe('Contains Inside of Resolved Object', function(done) {
    before(function(done) {
      xargs = {
        resolve: {
          followers: {
            format: "short",
            contains: {
              followers: {
                _id: mark._id.toString()
              }
            }
          }
        }
      };
      req = helpers.basicRequestObject(xargs);

      new Twine('User', {_id: DJ._id.toString()}, req, null, function(err, res) {
        result = res;
        error = err;
        done();
      });
    });

    it('should not return an error', function(done) {
      debugger;
      assert.isFalse(error);
      done();
    });

    it('should have a resolved User Object', function(done) {
      assert.property(result.resolve, 'User');
      assert.isObject(result.resolve.User);
      done();
    });

    it('should have an more than 1 followers object in result data', function(done) {
      assert.isTrue(result.data[0].followers.length > 1);
      done();
    });

    it('should have added a followers property to each resolved User', function(done) {
      var user      = result.resolve.User;
      var user_keys = _.keys(user);
      for(var key in user_keys) {
        assert.property(user[key], 'followers');
      }
      done();
    });

    it('should have an empty followers property except for the control user sean', function(done) {
      var control = sean._id.toString();
      var user    = result.resolve.User;
      for(var key in user_keys) {
        
      };
      done();
    });

  });
});
