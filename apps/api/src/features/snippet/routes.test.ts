import request from 'supertest'
import { createApp } from '../../app'

describe('Snippet routes test suite', () => {
   it('Test POST /snippet with correct request body', async () => {
      const app = await createApp()
      const reqBody = {
         title: 'new snippet',
         description: 'how to sort an array',
         author: 1,
         content: 'array.sort()',
      }
      const { body, status } = await request(app).post('/snippet').send(reqBody)

      expect(status).toBe(200)
      expect(body.snippet).toBeDefined()
      expect(body.snippet.title).toBe('new snippet')
      expect(body.snippet.description).toBe('how to sort an array')
      expect(body.snippet.author).toBe('1')
      expect(body.snippet.content).toBe('array.sort()')
   })
})
