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
var __assign = (this && this.__assign) || function () {
    __assign = Object.assign || function(t) {
        for (var s, i = 1, n = arguments.length; i < n; i++) {
            s = arguments[i];
            for (var p in s) if (Object.prototype.hasOwnProperty.call(s, p))
                t[p] = s[p];
        }
        return t;
    };
    return __assign.apply(this, arguments);
};
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
exports.register = void 0;
var function_1 = require("fp-ts/function");
var Either_1 = require("fp-ts/lib/Either");
var O = __importStar(require("fp-ts/Option"));
var TE = __importStar(require("fp-ts/TaskEither"));
var I = __importStar(require("io-ts"));
var ts_custom_error_1 = require("ts-custom-error");
var bcrypt_1 = require("../../infrastructure/bcrypt");
var error_1 = require("../../infrastructure/error");
var userRepo_1 = require("./userRepo");
var UserAlreadyExists = /** @class */ (function (_super) {
    __extends(UserAlreadyExists, _super);
    function UserAlreadyExists() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.status = 400;
        _this.code = 'UserAlreadyExists';
        _this.log = true;
        return _this;
    }
    return UserAlreadyExists;
}(ts_custom_error_1.CustomError));
var RegisterBody = I.interface({
    username: I.string,
    email: I.string,
    password: I.string,
});
var register = function (env, rawBody) {
    return function_1.pipe(TE.fromEither(function_1.pipe(RegisterBody.decode(rawBody), Either_1.mapLeft(function (_) { return new error_1.InvalidRequest(); }))), TE.chain(function (body) {
        return function_1.pipe(validateBody(body), TE.fromOption(function () { return new error_1.ValidationFailed(); }));
    }), TE.chain(function (dto) { return tryInsertUser(dto, env.pool); }));
};
exports.register = register;
var tryInsertUser = function (dto, pool) {
    return function_1.pipe(userRepo_1.findUserByEmail(dto.email, pool), TE.alt(function () { return userRepo_1.findUserByUsername(dto.password, pool); }), TE.chain(function (maybeUser) {
        return function_1.pipe(maybeUser, O.fold(function () { return TE.right(maybeUser); }, function () { return TE.left(new UserAlreadyExists()); }));
    }), TE.chain(function () { return bcrypt_1.hashPassword(dto.password); }), TE.chain(function (hashedPassword) { return userRepo_1.insertUser(__assign(__assign({}, dto), { password: hashedPassword }), pool); }));
};
var validateBody = function (body) {
    return function_1.pipe(O.of(body), O.filter(function (x) { return x.username.length >= 2; }), O.filter(function (x) { return x.username.length < 99; }), O.filter(function (x) { return x.email.includes('@'); }), O.filter(function (x) { return x.password.length >= 6; }), O.map(function (x) { return ({ username: x.username, password: x.password, email: x.email }); }));
};
