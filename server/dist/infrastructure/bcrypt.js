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
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    Object.defineProperty(o, k2, { enumerable: true, get: function() { return m[k]; } });
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __setModuleDefault = (this && this.__setModuleDefault) || (Object.create ? (function(o, v) {
    Object.defineProperty(o, "default", { enumerable: true, value: v });
}) : function(o, v) {
    o["default"] = v;
});
var __importStar = (this && this.__importStar) || function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null) for (var k in mod) if (k !== "default" && Object.prototype.hasOwnProperty.call(mod, k)) __createBinding(result, mod, k);
    __setModuleDefault(result, mod);
    return result;
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.hashPassword = exports.comparePasswords = void 0;
var bcrypt_1 = require("bcrypt");
var TE = __importStar(require("fp-ts/TaskEither"));
var ts_custom_error_1 = require("ts-custom-error");
var uuid_1 = require("uuid");
var BcryptError = /** @class */ (function (_super) {
    __extends(BcryptError, _super);
    function BcryptError() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.status = 500;
        _this.code = uuid_1.v4();
        _this.log = true;
        return _this;
    }
    return BcryptError;
}(ts_custom_error_1.CustomError));
var comparePasswords = function (hashedPassword, attempt) {
    return TE.tryCatch(function () { return bcrypt_1.compare(attempt, hashedPassword); }, function (_) { return new BcryptError('error while comparing passwords'); });
};
exports.comparePasswords = comparePasswords;
var hashPassword = function (password) {
    return TE.tryCatch(function () { return bcrypt_1.hash(password, 10); }, function (_) { return new BcryptError('error while hashing password'); });
};
exports.hashPassword = hashPassword;
