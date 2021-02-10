import request from 'supertest'
import { createApp } from '../../app'

describe('Snippet routes test suite', () => {
   it('Test POST /snippet with correct request body', async () => {
      const app = await createApp()

      const { body, status } = await request(app).get('/snippets')

      expect(status).toBe(200)
      expect(body.templates).toBeDefined()
   })
})
