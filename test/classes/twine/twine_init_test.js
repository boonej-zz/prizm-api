/**
 * Twine Initialization Tests
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


describe('Twine Initialization', function() {
  //classes that need to be tested before property/defaultbehavior testing
  //
  // $__digestHeaderArguments()
  // $__optExists()
  // $__parse()
  // $__resolveFilterProperties()
  var header = {string: "string", num: 234563, ob: {a:1, b:2, c:3}, arr: [0,1,2,3]};
  var req = helpers.basicRequestObject(header);

  describe('Private Initialization Functions', function(done) {

    describe('$__digestHeaderArguments()', function(done) {
      it('should return null if no request object is passed', function(done) {
        assert.isNull(Twine.prototype.$__digestHeaderArguments(null));
        done();
      });

      it('should parse base64 encoded x-args header and return object', function(done) {
        var digest_result = Twine.prototype.$__digestHeaderArguments(req);
        //assert properties exist
        assert.isObject(digest_result);
        assert.property(digest_result, 'string');
        assert.property(digest_result, 'num');
        assert.property(digest_result, 'ob');
        assert.property(digest_result, 'arr');

        //assert deep equal to test object
        assert.deepEqual(digest_result, header);
        done();
      });
    });

    describe('$__optExists()', function(done) {
      var options     = new Twine('User', {}, req, {page: 2, sort: 'test'}, null);
      var no_options  = new Twine('User', {}, req, null, null);

      it('should return true if a key exists in options property', function(done) {
        assert.isTrue(options.$__optExists('page'));
        assert.isTrue(options.$__optExists('sort'));
        done();
      });

      it('should return false if a key does not exist in options property', function(done) {
        assert.isFalse(options.$__optExists('bogus'));
        done();
      });

      it('should return false if no options are passed', function(done) {
        assert.isFalse(no_options.$__optExists('sort'));
        done();
      });
    });

    describe('$__parse()', function(done) {
      var default_value = 'test_default';
      var xargs = {};
      var request = req;
      var query = {test: "test"};
      var twine = new Twine('User', {}, request, null, null);
      var twine_query = new Twine('User', {}, {query: query}, null, null);

      it('should parse and return args value when present', function(done) {
        var actual = twine.$__parse('string');
        var expected = header.string;
        assert.equal(actual, expected);
        done();
      });

      it('should parse and return default value when option value does not exist', function(done) {
        var actual = twine.$__parse('this_function_is_dumb', default_value);
        assert.equal(actual, default_value);
        done();
      });

      it('should parse and return query value when present', function(done) {
        var actual = twine_query.$__parse('test');
        var expected = query.test;
        assert.equal(actual, expected);
        done();
      });

      it('should return default value with query option does not exist', function(done) {
        var actual = twine_query.$__parse('this_is_never_used_in_this_project', default_value);
        assert.equal(actual, default_value);
        done();
      });
    });

    describe('$__resolveFilterProperties()', function(done) {
      it('can be done later', function(done) {
        done();
      });
    });
  });

  describe('Twine Properties', function(done) {
    var twine = new Twine('User', {_id: '1324234'}, req, null, null);

    it('should have constructor properties', function(done) {
      assert.property(twine, 'Model');
      assert.property(twine, 'Schema');
      assert.property(twine, 'criteria');
      assert.property(twine, 'args');
      assert.property(twine, 'options');
      assert.property(twine, 'cb');
      assert.property(twine, 'limit');
      assert.property(twine, 'fields');
      assert.property(twine, 'page');
      assert.property(twine, 'page_by');
      assert.property(twine, 'page_direction');
      assert.property(twine, 'sort');
      assert.property(twine, 'sort_by');
      assert.property(twine, 'filters');
      assert.property(twine, 'contains');
      assert.property(twine, 'resolve');
      assert.property(twine, 'fetch');
      assert.property(twine, 'has_child_model');
      assert.property(twine, 'child_model');
      assert.property(twine, 'child_model_name');
      assert.property(twine, 'model_keys');
      done();
    });

    describe('Twine.Model', function(done) {
      it('Model should be an instance of Mongoose model object with a _doc property', function(done) {
        //TODO: need a better way of testng if this is an instance of a registered object
        assert.property(twine.Model(), '_doc');
        assert.property(twine.Model(), 'isNew');
        done();
      });
    });

    describe('Twine.Schema', function(done) {
      it('Schema should be a Mongoose.Model.Schema object with a paths property', function(done) {
        assert.property(twine.Schema, 'paths');
        done();
      });
    });

    describe('Twine.criteria', function(done) {
      it('should be set as initial find params', function(done) {
        assert.deepEqual(twine.criteria, {_id: '1324234'});
        done();
      });
    });

    describe('Twine.Request', function(done) {
      it('should be a request object with at least a x-args header property', function(done) {
        assert.property(twine.Request.headers, 'x-arguments');
        done();
      });
    });

    describe('Twine.args', function(done) {
      it('should be the parsed representation of x-args header property', function(done) {
        assert.deepEqual(twine.args, header);
        done();
      });
    });

    describe('Twine.options', function(done) {
      it('should be equal to the options object passed in constructor', function(done) {
        assert.isNull(twine.options);
        assert.deepEqual((new Twine('User', {}, req, {test: 'test'}, null)).options, {test: 'test'});
        done();
      });
    });

    describe('Twine.limit', function(done) {
      it('should be set default when not passed in request or options', function(done) {
        assert.equal(twine.limit, 30);
        done();
      });

      it('should be set from options params when passed', function(done) {
        assert.equal((new Twine('User', {}, req, {limit:5}, null)).limit, 5);
        done();
      });

      it('should be set from request when passed', function(done) {
        var h = {limit:8};
        var reqq = helpers.basicRequestObject(h);
        var twinel = new Twine('User', {}, reqq, null, null);
        assert.equal(twinel.limit, h.limit);
        done();
      });
    });

    describe('Twine.fields', function(done) {
      var h = {fields: "_id create_date modify_date"};
      it('should set fields as array or string if passed in request', function(done) {
        var reqq = helpers.basicRequestObject(h);
        var twinef = new Twine('User', {}, reqq, null, null);
        assert.deepEqual(twinef.fields, h.fields);
        done();
      });

      it('should set fields as array or string if passed in options', function(done) {
        var twinef = new Twine('User', {}, req, h, null);
        assert.deepEqual(twinef.fields, h.fields);
        done();
      });

      it('should be null if no fields param is passed in request or options', function(done) {
        assert.isNull(twine.fields);
        done();
      });
    });

    describe('Twine.page', function(done) {
      var h = {page: Date.now()};
      it('should set the page property if passed in request', function(done) {
        var reqq = helpers.basicRequestObject(h);
        var twinep = new Twine('User', {}, reqq, null, null);
        assert.deepEqual(twinep.page, h.page);
        done();
      });

      it('should set the page property if passed in options', function(done) {
        var twinep = new Twine('User', {}, req, h, null);
        assert.deepEqual(twinep.page, h.page);
        done();
      });

      it('should be null if no page params is passed in request or options', function(done) {
        assert.isNull(twine.page);
        done();
      });
    });

    describe('Twine.page_by', function(done) {
      var h = {page_by: 'delete_date'};
      it('should set the page_by property if passed in request', function(done) {
        var reqq = helpers.basicRequestObject(h);
        var twinep = new Twine('User', {}, reqq, null, null);
        assert.deepEqual(twinep.page_by, h.page_by);
        done();
      });

      it('should set the page_by property if passed in options', function(done) {
        var twinep = new Twine('User', {}, req, h, null);
        assert.deepEqual(twinep.page_by, h.page_by);
        done();
      });

      it('should equal default if no page_by params is passed in request or options', function(done) {
        assert.equal(twine.page_by, 'create_date');
        done();
      });
    });

    describe('Twine.page_direction', function(done) {
      var h = {page_direction: 1};
      it('should set the page_direction property if passed in request', function(done) {
        var reqq = helpers.basicRequestObject(h);
        var twinep = new Twine('User', {}, reqq, null, null);
        assert.deepEqual(twinep.page_direction, h.page_direction);
        done();
      });

      it('should set the page_direction property if passed in options', function(done) {
        var twinep = new Twine('User', {}, req, h, null);
        assert.deepEqual(twinep.page_direction, h.page_direction);
        done();
      });

      it('should equal default if no page_direction params is passed in request or options', function(done) {
        assert.deepEqual(twine.page_direction, -1);
        done();
      });
    });

    describe('Twine.sort', function(done) {
      var h = {sort: 1};
      it('should set the property if passed in request', function(done) {
        var reqq = helpers.basicRequestObject(h);
        var twines = new Twine('User', {}, reqq, null, null);
        assert.deepEqual(twines.sort, h.sort);
        done();
      });

      it('should set the sort property if passed in options', function(done) {
        var twines = new Twine('User', {}, req, h, null);
        assert.deepEqual(twines.sort, h.sort);
        done();
      });

      it('should equal page_direction if no sort param is passed in request or options', function(done) {
        assert.deepEqual(twine.sort, twine.page_direction);
        done();
      });
    });

    describe('Twine.sort_by', function(done) {
      var h = {sort_by: "delete_date"};
      it('should set the sort_by property if passed in request', function(done) {
        var reqq = helpers.basicRequestObject(h);
        var twines = new Twine('User', {}, reqq, null, null);
        assert.deepEqual(twines.sort_by, h.sort_by);
        done();
      });

      it('should set the sort_by property if passed in options', function(done) {
        var twines = new Twine('User', {}, req, h, null);
        assert.deepEqual(twines.sort_by, h.sort_by);
        done();
      });

      it('should equal page_by if no sort param is passed in request or options', function(done) {
        assert.deepEqual(twine.sort_by, twine.page_by);
        done();
      });
    });

    describe('Twine.filters', function(done) {
      var h = {active: true, type: 'user', test: 'test', noway: 'jose'};
      var reqq = helpers.basicRequestObject(h);
      var twinef = new Twine('User', {}, reqq, null, null);
      var twinefo = new Twine('User', {}, req, h, null);

      it('should set the property if passed in request', function(done) {
        assert.isObject(twinef.filters);
        assert.isTrue(_.keys(twinef.filters).length > 0);
        done();
      });


      //TODO: this should be added once initial base is in place
      it.skip('should set the property if passed in options', function(done) {
        assert.isObject(twinefo.filters);
        assert.isTrue(_.keys(twinefo.filters).length > 0);
        done();
      });

      it('should only set filters that are actual model properties', function(done) {
        assert.property(twinef.filters, 'active');
        assert.property(twinef.filters, 'type');
        assert.notProperty(twinef.filters, 'test');
        assert.notProperty(twinef.filters, 'noway');
        done();
      });

      it('should be empty if no filters are passed', function(done) {
        assert.isTrue(_.isEmpty(twine.filters));
        done();
      });
    });

    describe('Twine.contains', function(done) {
      var h = {contains: {active: true, type: 'user'}};
      var reqq = helpers.basicRequestObject(h);
      var twinec = new Twine('User', {}, reqq, null, null);
      var twineco = new Twine('User', {}, req, h, null);

      it('should set the property if passed in request', function(done) {
        assert.deepEqual(twinec.contains, h.contains);
        done();
      });

      it('should set the property if passed in options', function(done) {
        assert.deepEqual(twineco.contains, h.contains);
        done();
      });

      it('should be null if no contains are passed', function(done) {
        assert.isNull(twine.contains);
        done();
      });
    });

    describe('Twine.resolve', function(done) {
      var h = {resolve: { from: { format: "short"}}};
      var reqq = helpers.basicRequestObject(h);
      var twiner = new Twine('User', {}, reqq, null, null);
      var twinero = new Twine('User', {}, req, h, null);

      it('should set the property if passed in request', function(done) {
        assert.deepEqual(twiner.resolve, h.resolve);
        done();
      });

      it('should set the property if passed in options', function(done) {
        assert.deepEqual(twinero.resolve, h.resolve);
        done();
      });

      it('should be null if no resolve objects are passed', function(done) {
        assert.isNull(twine.resolve);
        done();
      });
    });

    describe('Twine.fetch', function(done) {
      it('should be null', function(done) {
        assert.isNull(twine.fetch);
        done();
      });
    });

    var child_model_option = {is_child_model: true, child_model: 'Comment'};
    var twine_child = new Twine('Post', {}, req, child_model_option, null);
    
    describe('Twine.has_child_model', function(done) {
      it('should be set to true if is_child_model is passed in options', function(done) {
        assert.isTrue(twine_child.has_child_model);
        done();
      });

      it('should be null if option is not set', function(done) {
        assert.isNull(twine.has_child_model);
        done();
      });
    });

    describe('Twine.child_model_name', function(done) {
      it('should be set if child_model is passed in options', function(done) {
        assert.equal(twine_child.child_model_name, child_model_option.child_model);
        done();
      });

      it('should be null if option is not set', function(done) {
        assert.isNull(twine.child_model_name);
        done();
      });
    });

    describe('Twine.child_model', function(done) {
      it('should be set as an object if child_model is passed in options', function(done) {
        assert.isObject(twine_child.child_model());
        done();
      });

      it('should be an instance of Mongoose Model object', function(done) {
        //TODO: need a better way to test this is an instance of mongoose.model();
        var child = twine_child.child_model();
        assert.property(child, '_doc');
        done();
      });
    });

    describe('Twine.model_keys', function(done) {
      //TODO: needs more unit tests
      it('should not be null', function(done) {
        assert.isNotNull(twine.model_keys);
        done();
      });
    });
  });
});
