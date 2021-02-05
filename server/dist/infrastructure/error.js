"use strict";
var __extends = (this && this.__extends) || (function () {
    var extendStatics = function (d, b) {
        extendStatics = Object.setPrototypeOf ||
            ({ __proto__: [] } instanceof Array && function (d, b) { d.__proto__ = b; }) ||
            function (d, b) { for (var p in b) if (Object.prototype.hasOwnProperty.call(b, p)) d[p] = b[p]; };
        return extendStatics(d, b);
    };
    return function (d, b) {
        extendStatics(d, b);
        function __() { this.constructor = d; }
        d.prototype = b === null ? Object.create(b) : (__.prototype = b.prototype, new __());
    };
})();
Object.defineProperty(exports, "__esModule", { value: true });
exports.ValidationFailed = exports.InvalidRequest = exports.processError = void 0;
var ts_custom_error_1 = require("ts-custom-error");
var logger_1 = require("./logger");
var uuid_1 = require("uuid");
var processError = function (res) { return function (err) {
    if (!err.code && !err.status) {
        err = new UnexpectedError(err.message);
    }
    if (err.log) {
        logger_1.logger.error(err);
    }
    res.status(err.status).json({ code: err.code });
}; };
exports.processError = processError;
// General errors
var UnexpectedError = /** @class */ (function (_super) {
    __extends(UnexpectedError, _super);
    function UnexpectedError() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.status = 500;
        _this.code = uuid_1.v4();
        _this.log = true;
        return _this;
    }
    return UnexpectedError;
}(ts_custom_error_1.CustomError));
var InvalidRequest = /** @class */ (function (_super) {
    __extends(InvalidRequest, _super);
    function InvalidRequest() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.status = 500;
        _this.code = "InvalidRequest";
        _this.log = false;
        return _this;
    }
    return InvalidRequest;
}(ts_custom_error_1.CustomError));
exports.InvalidRequest = InvalidRequest;
var ValidationFailed = /** @class */ (function (_super) {
    __extends(ValidationFailed, _super);
    function ValidationFailed() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.status = 500;
        _this.code = "ValidationFailed";
        _this.log = false;
        return _this;
    }
    return ValidationFailed;
}(ts_custom_error_1.CustomError));
exports.ValidationFailed = ValidationFailed;
