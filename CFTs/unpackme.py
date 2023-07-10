import base64
from cryptography.fernet import Fernet



payload = b'gAAAAABkESVQgcBx4lIJYH8CqVaBgXSMgXXHob0wFktjJtjWu0qkainfdmS7Nt1GZ6O0ssH1sGVZYpHPrvTILpVnKSlka6NPJvvoTHKA6u4leSMxdBE9oeu0Ly5onZlS3uo4-UoS4jIuA0zKtlr9rGqBXEOMIaR7ClrdE41uLFMRYtYE_9nuXA453hnA5Bx1-YsDDm4WQlaKMaCbMuqG-m2SxYvGqCRM0jkEp1Wi3o8YWZ6LKgIMkvkywi2fcyUbRQhE8ucehVGS9QR1BzM28cbwff1QjxW5hw=='

key_str = 'correctstaplecorrectstaplecorrec'
key_base64 = base64.b64encode(key_str.encode())
f = Fernet(key_base64)
plain = f.decrypt(payload)
print(plain.decode())