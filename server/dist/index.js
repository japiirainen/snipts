"use strict";
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
var __generator = (this && this.__generator) || function (thisArg, body) {
    var _ = { label: 0, sent: function() { if (t[0] & 1) throw t[1]; return t[1]; }, trys: [], ops: [] }, f, y, t, g;
    return g = { next: verb(0), "throw": verb(1), "return": verb(2) }, typeof Symbol === "function" && (g[Symbol.iterator] = function() { return this; }), g;
    function verb(n) { return function (v) { return step([n, v]); }; }
    function step(op) {
        if (f) throw new TypeError("Generator is already executing.");
        while (_) try {
            if (f = 1, y && (t = op[0] & 2 ? y["return"] : op[0] ? y["throw"] || ((t = y["return"]) && t.call(y), 0) : y.next) && !(t = t.call(y, op[1])).done) return t;
            if (y = 0, t) op = [op[0] & 2, t.value];
            switch (op[0]) {
                case 0: case 1: t = op; break;
                case 4: _.label++; return { value: op[1], done: false };
                case 5: _.label++; y = op[1]; op = [0]; continue;
                case 7: op = _.ops.pop(); _.trys.pop(); continue;
                default:
                    if (!(t = _.trys, t = t.length > 0 && t[t.length - 1]) && (op[0] === 6 || op[0] === 2)) { _ = 0; continue; }
                    if (op[0] === 3 && (!t || (op[1] > t[0] && op[1] < t[3]))) { _.label = op[1]; break; }
                    if (op[0] === 6 && _.label < t[1]) { _.label = t[1]; t = op; break; }
                    if (t && _.label < t[2]) { _.label = t[2]; _.ops.push(op); break; }
                    if (t[2]) _.ops.pop();
                    _.trys.pop(); continue;
            }
            op = body.call(thisArg, _);
        } catch (e) { op = [6, e]; y = 0; } finally { f = t = 0; }
        if (op[0] & 5) throw op[1]; return { value: op[0] ? op[1] : void 0, done: true };
    }
};
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
var cookie_parser_1 = __importDefault(require("cookie-parser"));
var cors_1 = __importDefault(require("cors"));
var express_1 = __importDefault(require("express"));
var morgan_1 = __importDefault(require("morgan"));
var authMiddleware_1 = require("./features/auth/authMiddleware");
var authRoutes_1 = require("./features/auth/authRoutes");
var config_1 = require("./infrastructure/config");
var db_1 = require("./infrastructure/db");
var env_1 = require("./infrastructure/env");
var logger_1 = require("./infrastructure/logger");
var createApp = function () { return __awaiter(void 0, void 0, void 0, function () {
    var prerequisites, pool, app;
    return __generator(this, function (_a) {
        switch (_a.label) {
            case 0:
                prerequisites = [db_1.createDbPool(), Promise.resolve()];
                return [4 /*yield*/, Promise.all(prerequisites)];
            case 1:
                pool = (_a.sent())[0];
                app = express_1.default();
                app.use(morgan_1.default('dev'))
                    .use(cors_1.default({
                    credentials: true,
                    origin: 'http://localhost:4000',
                }))
                    .use(express_1.default.json())
                    .use(cookie_parser_1.default())
                    .get('/health', function (_, res) {
                    if (pool) {
                        res.status(200).json({ status: 'healthy' });
                    }
                    else {
                        res.status(503).json({ status: 'unavailable' });
                    }
                })
                    .use(env_1.initializeEnv(pool))
                    .get('/me', authMiddleware_1.requireUser, authRoutes_1.authRoutes.me)
                    .post('login', authRoutes_1.authRoutes.login)
                    .post('/logout', authRoutes_1.authRoutes.logout)
                    .post('/register', authRoutes_1.authRoutes.register)
                    .post('/refresh-token', authRoutes_1.authRoutes.refreshToken);
                return [2 /*return*/, app];
        }
    });
}); };
createApp().then(function (app) {
    return app.listen(config_1.config.application.port, function () {
        return logger_1.logger.info(config_1.config.application.name + " is listening on " + config_1.config.application.port);
    });
});
