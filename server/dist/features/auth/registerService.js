"use strict";
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
const function_1 = require("fp-ts/function");
const Either_1 = require("fp-ts/lib/Either");
const O = __importStar(require("fp-ts/Option"));
const TE = __importStar(require("fp-ts/TaskEither"));
const I = __importStar(require("io-ts"));
const ts_custom_error_1 = require("ts-custom-error");
const bcrypt_1 = require("../../infrastructure/bcrypt");
const error_1 = require("../../infrastructure/error");
const userRepo_1 = require("./userRepo");
class UserAlreadyExists extends ts_custom_error_1.CustomError {
    constructor() {
        super(...arguments);
        this.status = 400;
        this.code = 'UserAlreadyExists';
        this.log = true;
    }
}
const RegisterBody = I.interface({
    username: I.string,
    email: I.string,
    password: I.string,
});
const register = (env, rawBody) => {
    return function_1.pipe(TE.fromEither(function_1.pipe(RegisterBody.decode(rawBody), Either_1.mapLeft(_ => new error_1.InvalidRequest()))), TE.chain(body => function_1.pipe(validateBody(body), TE.fromOption(() => new error_1.ValidationFailed()))), TE.chain(dto => tryInsertUser(dto, env.pool)));
};
exports.register = register;
const tryInsertUser = (dto, pool) => function_1.pipe(userRepo_1.findUserByEmail(dto.email, pool), TE.alt(() => userRepo_1.findUserByUsername(dto.password, pool)), TE.chain(maybeUser => function_1.pipe(maybeUser, O.fold(() => TE.right(maybeUser), () => TE.left(new UserAlreadyExists())))), TE.chain(() => bcrypt_1.hashPassword(dto.password)), TE.chain(hashedPassword => userRepo_1.insertUser({ ...dto, password: hashedPassword }, pool)));
const validateBody = (body) => function_1.pipe(O.of(body), O.filter(x => x.username.length >= 2), O.filter(x => x.username.length < 99), O.filter(x => x.email.includes('@')), O.filter(x => x.password.length >= 6), O.map(x => ({ username: x.username, password: x.password, email: x.email })));
