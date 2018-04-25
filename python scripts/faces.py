import face_recognition
import glob
face_encoding = []
for filename in glob.glob('./Scripts/faces/data/*.jpg'):
  image = face_recognition.load_image_file(filename)
  face_encoding.append(face_recognition.face_encodings(image, num_jitters = 10)[0])

dists = []
for dist in range(0, len(face_encoding)):
  x = face_recognition.face_distance(face_encoding, face_encoding[dist])
  dists.append(x)
  print(x)

import numpy
numpy.savetxt("foo.csv", face_encoding, delimiter=",")
numpy.savetxt("foo2.csv", dists, delimiter=",") 
