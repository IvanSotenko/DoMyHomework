﻿module DoMyHomework.Tests.TestMatrix

// Matrix from SuiteSparse Matrix Collection
// link: https://sparse.tamu.edu/Newman/football
// name: football
// size: 115x115
let testMatrix1Arr =
    [| (2, 1, 1)
       (5, 1, 1)
       (10, 1, 1)
       (17, 1, 1)
       (24, 1, 1)
       (34, 1, 1)
       (36, 1, 1)
       (42, 1, 1)
       (66, 1, 1)
       (91, 1, 1)
       (94, 1, 1)
       (105, 1, 1)
       (26, 2, 1)
       (28, 2, 1)
       (34, 2, 1)
       (38, 2, 1)
       (46, 2, 1)
       (58, 2, 1)
       (90, 2, 1)
       (102, 2, 1)
       (104, 2, 1)
       (106, 2, 1)
       (110, 2, 1)
       (4, 3, 1)
       (7, 3, 1)
       (14, 3, 1)
       (15, 3, 1)
       (16, 3, 1)
       (48, 3, 1)
       (61, 3, 1)
       (65, 3, 1)
       (73, 3, 1)
       (75, 3, 1)
       (101, 3, 1)
       (107, 3, 1)
       (6, 4, 1)
       (12, 4, 1)
       (27, 4, 1)
       (41, 4, 1)
       (53, 4, 1)
       (59, 4, 1)
       (73, 4, 1)
       (75, 4, 1)
       (82, 4, 1)
       (85, 4, 2)
       (103, 4, 1)
       (6, 5, 1)
       (10, 5, 1)
       (17, 5, 1)
       (24, 5, 1)
       (29, 5, 1)
       (42, 5, 1)
       (70, 5, 1)
       (94, 5, 1)
       (105, 5, 1)
       (109, 5, 1)
       (11, 6, 1)
       (12, 6, 1)
       (53, 6, 1)
       (75, 6, 1)
       (82, 6, 1)
       (85, 6, 1)
       (91, 6, 1)
       (98, 6, 1)
       (99, 6, 1)
       (108, 6, 1)
       (8, 7, 1)
       (33, 7, 1)
       (40, 7, 1)
       (48, 7, 1)
       (56, 7, 1)
       (59, 7, 1)
       (61, 7, 1)
       (65, 7, 1)
       (86, 7, 1)
       (101, 7, 1)
       (107, 7, 1)
       (9, 8, 1)
       (22, 8, 1)
       (23, 8, 1)
       (41, 8, 1)
       (69, 8, 1)
       (74, 8, 1)
       (78, 8, 1)
       (79, 8, 1)
       (83, 8, 1)
       (109, 8, 1)
       (112, 8, 1)
       (10, 9, 1)
       (22, 9, 1)
       (23, 9, 1)
       (42, 9, 1)
       (52, 9, 1)
       (69, 9, 1)
       (78, 9, 1)
       (79, 9, 1)
       (91, 9, 1)
       (112, 9, 1)
       (17, 10, 1)
       (23, 10, 1)
       (24, 10, 1)
       (42, 10, 1)
       (65, 10, 1)
       (94, 10, 1)
       (105, 10, 1)
       (109, 10, 1)
       (12, 11, 1)
       (61, 11, 1)
       (73, 11, 1)
       (75, 11, 1)
       (82, 11, 1)
       (85, 11, 1)
       (99, 11, 1)
       (103, 11, 1)
       (108, 11, 1)
       (25, 12, 1)
       (29, 12, 1)
       (51, 12, 1)
       (70, 12, 1)
       (91, 12, 1)
       (98, 12, 1)
       (105, 12, 1)
       (14, 13, 1)
       (15, 13, 1)
       (18, 13, 1)
       (19, 13, 1)
       (27, 13, 1)
       (35, 13, 1)
       (37, 13, 1)
       (39, 13, 1)
       (44, 13, 1)
       (86, 13, 1)
       (16, 14, 1)
       (33, 14, 1)
       (40, 14, 1)
       (46, 14, 1)
       (61, 14, 1)
       (65, 14, 1)
       (101, 14, 1)
       (107, 14, 1)
       (111, 14, 1)
       (16, 15, 1)
       (27, 15, 1)
       (39, 15, 1)
       (44, 15, 1)
       (55, 15, 1)
       (72, 15, 1)
       (86, 15, 1)
       (100, 15, 2)
       (33, 16, 1)
       (40, 16, 1)
       (48, 16, 1)
       (61, 16, 1)
       (69, 16, 1)
       (93, 16, 1)
       (101, 16, 1)
       (107, 16, 1)
       (115, 16, 1)
       (18, 17, 1)
       (24, 17, 1)
       (39, 17, 1)
       (42, 17, 1)
       (68, 17, 1)
       (82, 17, 1)
       (94, 17, 1)
       (105, 17, 1)
       (21, 18, 1)
       (28, 18, 2)
       (59, 18, 1)
       (63, 18, 1)
       (66, 18, 1)
       (88, 18, 1)
       (96, 18, 1)
       (97, 18, 1)
       (114, 18, 1)
       (20, 19, 1)
       (32, 19, 1)
       (35, 19, 1)
       (37, 19, 1)
       (39, 19, 1)
       (43, 19, 1)
       (55, 19, 1)
       (62, 19, 1)
       (72, 19, 1)
       (100, 19, 1)
       (30, 20, 1)
       (31, 20, 1)
       (34, 20, 1)
       (36, 20, 1)
       (37, 20, 1)
       (45, 20, 1)
       (56, 20, 1)
       (80, 20, 1)
       (95, 20, 1)
       (102, 20, 1)
       (22, 21, 1)
       (37, 21, 1)
       (63, 21, 1)
       (66, 21, 1)
       (71, 21, 1)
       (76, 21, 1)
       (77, 21, 1)
       (88, 21, 1)
       (97, 21, 1)
       (114, 21, 1)
       (23, 22, 1)
       (33, 22, 1)
       (47, 22, 1)
       (52, 22, 1)
       (69, 22, 1)
       (78, 22, 1)
       (109, 22, 1)
       (112, 22, 1)
       (24, 23, 1)
       (48, 23, 1)
       (52, 23, 1)
       (69, 23, 1)
       (78, 23, 1)
       (79, 23, 1)
       (109, 23, 1)
       (42, 24, 1)
       (79, 24, 1)
       (91, 24, 1)
       (94, 24, 1)
       (105, 24, 1)
       (112, 24, 1)
       (26, 25, 1)
       (29, 25, 1)
       (51, 25, 1)
       (67, 25, 1)
       (70, 25, 1)
       (85, 25, 1)
       (88, 25, 1)
       (91, 25, 1)
       (111, 25, 1)
       (34, 26, 1)
       (38, 26, 1)
       (46, 26, 1)
       (54, 26, 1)
       (90, 26, 1)
       (104, 26, 1)
       (106, 26, 1)
       (107, 26, 1)
       (110, 26, 1)
       (28, 27, 1)
       (35, 27, 1)
       (39, 27, 1)
       (43, 27, 1)
       (44, 27, 1)
       (62, 27, 1)
       (86, 27, 1)
       (57, 28, 1)
       (63, 28, 1)
       (64, 28, 1)
       (66, 28, 1)
       (71, 28, 1)
       (77, 28, 1)
       (96, 28, 1)
       (97, 28, 1)
       (39, 29, 1)
       (51, 29, 1)
       (70, 29, 1)
       (79, 29, 1)
       (91, 29, 1)
       (114, 29, 1)
       (31, 30, 1)
       (36, 30, 1)
       (43, 30, 1)
       (56, 30, 1)
       (80, 30, 1)
       (81, 30, 1)
       (83, 30, 1)
       (92, 30, 1)
       (95, 30, 1)
       (102, 30, 1)
       (36, 31, 1)
       (45, 31, 1)
       (51, 31, 1)
       (56, 31, 1)
       (80, 31, 1)
       (83, 31, 1)
       (95, 31, 1)
       (102, 31, 1)
       (110, 31, 1)
       (33, 32, 1)
       (35, 32, 1)
       (44, 32, 1)
       (55, 32, 1)
       (56, 32, 1)
       (62, 32, 1)
       (72, 32, 1)
       (80, 32, 1)
       (86, 32, 1)
       (100, 32, 1)
       (40, 33, 1)
       (48, 33, 1)
       (50, 33, 1)
       (65, 33, 1)
       (101, 33, 1)
       (107, 33, 1)
       (38, 34, 1)
       (46, 34, 1)
       (90, 34, 1)
       (104, 34, 1)
       (106, 34, 1)
       (110, 34, 1)
       (36, 35, 1)
       (43, 35, 1)
       (55, 35, 1)
       (62, 35, 1)
       (72, 35, 1)
       (95, 35, 1)
       (100, 35, 1)
       (45, 36, 1)
       (56, 36, 1)
       (80, 36, 1)
       (93, 36, 1)
       (95, 36, 1)
       (102, 36, 1)
       (38, 37, 1)
       (44, 37, 1)
       (59, 37, 1)
       (60, 37, 1)
       (46, 38, 1)
       (81, 38, 1)
       (90, 38, 1)
       (96, 38, 1)
       (104, 38, 1)
       (106, 38, 1)
       (110, 38, 1)
       (40, 39, 1)
       (44, 39, 1)
       (55, 39, 1)
       (72, 39, 1)
       (86, 39, 1)
       (48, 40, 1)
       (55, 40, 1)
       (61, 40, 1)
       (83, 40, 1)
       (101, 40, 1)
       (107, 40, 1)
       (42, 41, 1)
       (52, 41, 1)
       (53, 41, 1)
       (73, 41, 1)
       (75, 41, 1)
       (82, 41, 1)
       (99, 41, 1)
       (103, 41, 1)
       (108, 41, 1)
       (68, 42, 1)
       (94, 42, 1)
       (105, 42, 1)
       (44, 43, 1)
       (58, 43, 1)
       (64, 43, 1)
       (62, 44, 1)
       (71, 44, 1)
       (80, 44, 1)
       (86, 44, 1)
       (46, 45, 1)
       (49, 45, 1)
       (58, 45, 1)
       (67, 45, 1)
       (76, 45, 1)
       (87, 45, 1)
       (92, 45, 1)
       (113, 45, 1)
       (63, 46, 1)
       (90, 46, 1)
       (104, 46, 1)
       (106, 46, 1)
       (110, 46, 1)
       (48, 47, 1)
       (50, 47, 1)
       (54, 47, 1)
       (68, 47, 1)
       (74, 47, 1)
       (84, 47, 1)
       (89, 47, 1)
       (111, 47, 1)
       (112, 47, 1)
       (115, 47, 1)
       (61, 48, 1)
       (62, 48, 1)
       (65, 48, 1)
       (101, 48, 1)
       (50, 49, 1)
       (54, 49, 1)
       (58, 49, 1)
       (67, 49, 1)
       (76, 49, 1)
       (87, 49, 1)
       (92, 49, 1)
       (93, 49, 1)
       (97, 49, 1)
       (99, 49, 1)
       (54, 50, 1)
       (68, 50, 1)
       (74, 50, 1)
       (84, 50, 1)
       (85, 50, 1)
       (89, 50, 1)
       (111, 50, 1)
       (115, 50, 1)
       (52, 51, 1)
       (69, 51, 1)
       (70, 51, 1)
       (79, 51, 1)
       (91, 51, 1)
       (69, 52, 1)
       (78, 52, 1)
       (79, 52, 1)
       (102, 52, 1)
       (109, 52, 1)
       (112, 52, 1)
       (54, 53, 1)
       (73, 53, 1)
       (75, 53, 1)
       (85, 53, 1)
       (99, 53, 1)
       (103, 53, 1)
       (113, 53, 1)
       (68, 54, 1)
       (74, 54, 1)
       (84, 54, 1)
       (87, 54, 1)
       (89, 54, 1)
       (111, 54, 1)
       (115, 54, 1)
       (56, 55, 1)
       (62, 55, 1)
       (72, 55, 1)
       (100, 55, 1)
       (80, 56, 1)
       (90, 56, 1)
       (95, 56, 1)
       (102, 56, 1)
       (58, 57, 1)
       (63, 57, 1)
       (66, 57, 1)
       (71, 57, 1)
       (77, 57, 1)
       (88, 57, 1)
       (96, 57, 1)
       (97, 57, 1)
       (107, 57, 1)
       (76, 58, 1)
       (87, 58, 1)
       (92, 58, 1)
       (93, 58, 1)
       (113, 58, 1)
       (60, 59, 1)
       (64, 59, 1)
       (89, 59, 1)
       (98, 59, 1)
       (102, 59, 1)
       (115, 59, 1)
       (61, 60, 1)
       (64, 60, 1)
       (67, 60, 1)
       (77, 60, 1)
       (98, 60, 1)
       (114, 60, 1)
       (65, 61, 1)
       (72, 61, 1)
       (107, 61, 1)
       (63, 62, 1)
       (72, 62, 1)
       (93, 62, 1)
       (100, 62, 1)
       (71, 63, 1)
       (77, 63, 1)
       (88, 63, 1)
       (96, 63, 1)
       (106, 63, 1)
       (65, 64, 1)
       (66, 64, 1)
       (98, 64, 1)
       (110, 64, 1)
       (113, 64, 1)
       (101, 65, 1)
       (107, 65, 1)
       (112, 65, 1)
       (67, 66, 1)
       (71, 66, 1)
       (88, 66, 1)
       (97, 66, 1)
       (114, 66, 1)
       (76, 67, 1)
       (77, 67, 1)
       (87, 67, 1)
       (92, 67, 1)
       (93, 67, 1)
       (113, 67, 1)
       (69, 68, 1)
       (74, 68, 1)
       (84, 68, 1)
       (89, 68, 1)
       (105, 68, 1)
       (111, 68, 1)
       (115, 68, 1)
       (79, 69, 1)
       (109, 69, 1)
       (112, 69, 1)
       (71, 70, 1)
       (84, 70, 1)
       (89, 70, 1)
       (91, 70, 1)
       (92, 70, 1)
       (96, 70, 1)
       (77, 71, 1)
       (96, 71, 1)
       (104, 71, 1)
       (114, 71, 1)
       (73, 72, 1)
       (100, 72, 1)
       (75, 73, 1)
       (82, 73, 1)
       (103, 73, 1)
       (105, 73, 1)
       (108, 73, 1)
       (75, 74, 1)
       (78, 74, 1)
       (84, 74, 1)
       (89, 74, 1)
       (111, 74, 1)
       (115, 74, 1)
       (83, 75, 1)
       (85, 75, 1)
       (103, 75, 1)
       (77, 76, 1)
       (87, 76, 1)
       (93, 76, 1)
       (108, 76, 1)
       (113, 76, 1)
       (96, 77, 1)
       (97, 77, 1)
       (114, 77, 1)
       (79, 78, 1)
       (83, 78, 1)
       (99, 78, 1)
       (109, 78, 1)
       (112, 78, 1)
       (109, 79, 1)
       (112, 79, 1)
       (81, 80, 1)
       (95, 80, 1)
       (102, 80, 1)
       (110, 80, 1)
       (83, 81, 1)
       (86, 81, 1)
       (87, 81, 1)
       (92, 81, 1)
       (94, 81, 1)
       (95, 81, 1)
       (106, 81, 1)
       (111, 81, 1)
       (83, 82, 1)
       (84, 82, 1)
       (85, 82, 1)
       (99, 82, 1)
       (108, 82, 1)
       (94, 83, 1)
       (95, 83, 1)
       (101, 83, 1)
       (85, 84, 1)
       (89, 84, 1)
       (111, 84, 1)
       (115, 84, 1)
       (99, 85, 1)
       (108, 85, 1)
       (100, 86, 1)
       (88, 87, 1)
       (92, 87, 1)
       (93, 87, 1)
       (98, 87, 1)
       (96, 88, 1)
       (97, 88, 1)
       (105, 88, 1)
       (114, 88, 1)
       (90, 89, 1)
       (108, 89, 1)
       (111, 89, 1)
       (115, 89, 1)
       (100, 90, 1)
       (104, 90, 1)
       (106, 90, 1)
       (110, 90, 1)
       (93, 92, 1)
       (94, 92, 1)
       (113, 92, 1)
       (107, 93, 1)
       (113, 93, 1)
       (105, 94, 1)
       (102, 95, 1)
       (114, 96, 1)
       (113, 97, 1)
       (114, 97, 1)
       (99, 98, 1)
       (113, 98, 1)
       (103, 99, 1)
       (108, 99, 1)
       (101, 100, 1)
       (103, 101, 1)
       (104, 103, 1)
       (108, 103, 1)
       (106, 104, 1)
       (110, 104, 1)
       (115, 105, 1)
       (110, 106, 1)
       (112, 109, 1)
       (115, 111, 1) |]


// Matrix from SuiteSparse Matrix Collection
// link: https://sparse.tamu.edu/HB/lnsp_131
// name: lnsp_131
// size: 131x131
let testMatrix2Arr =
    [| (1, 1, 1)
       (9, 1, 1)
       (32, 1, 1)
       (2, 2, 1)
       (3, 3, 1)
       (4, 4, 1)
       (5, 5, 1)
       (9, 5, 1)
       (14, 5, 1)
       (30, 5, 1)
       (32, 5, 1)
       (37, 5, 1)
       (6, 6, 1)
       (31, 6, 1)
       (7, 7, 1)
       (9, 7, 1)
       (32, 7, 1)
       (8, 8, 1)
       (33, 8, 1)
       (32, 9, 1)
       (10, 10, 1)
       (14, 10, 1)
       (19, 10, 1)
       (35, 10, 1)
       (37, 10, 1)
       (42, 10, 1)
       (11, 11, 1)
       (36, 11, 1)
       (12, 12, 1)
       (14, 12, 1)
       (37, 12, 1)
       (13, 13, 1)
       (38, 13, 1)
       (37, 14, 1)
       (15, 15, 1)
       (19, 15, 1)
       (24, 15, 1)
       (40, 15, 1)
       (42, 15, 1)
       (47, 15, 1)
       (16, 16, 1)
       (41, 16, 1)
       (17, 17, 1)
       (19, 17, 1)
       (42, 17, 1)
       (18, 18, 1)
       (43, 18, 1)
       (42, 19, 1)
       (20, 20, 1)
       (24, 20, 1)
       (47, 20, 1)
       (21, 21, 1)
       (22, 22, 1)
       (24, 22, 1)
       (47, 22, 1)
       (23, 23, 1)
       (47, 24, 1)
       (25, 25, 1)
       (26, 26, 1)
       (30, 26, 1)
       (32, 26, 1)
       (34, 26, 1)
       (57, 26, 1)
       (27, 27, 1)
       (31, 27, 1)
       (3, 28, 1)
       (28, 28, 1)
       (32, 28, 1)
       (29, 29, 1)
       (33, 29, 1)
       (30, 30, 1)
       (31, 30, 1)
       (32, 30, 1)
       (33, 30, 1)
       (34, 30, 1)
       (35, 30, 1)
       (37, 30, 1)
       (39, 30, 1)
       (55, 30, 1)
       (57, 30, 1)
       (62, 30, 1)
       (27, 31, 1)
       (30, 31, 1)
       (31, 31, 1)
       (36, 31, 1)
       (56, 31, 1)
       (7, 32, 1)
       (9, 32, 1)
       (28, 32, 1)
       (32, 32, 1)
       (34, 32, 1)
       (37, 32, 1)
       (57, 32, 1)
       (29, 33, 1)
       (30, 33, 1)
       (33, 33, 1)
       (38, 33, 1)
       (58, 33, 1)
       (30, 34, 1)
       (32, 34, 1)
       (57, 34, 1)
       (30, 35, 1)
       (35, 35, 1)
       (36, 35, 1)
       (37, 35, 1)
       (38, 35, 1)
       (39, 35, 1)
       (40, 35, 1)
       (42, 35, 1)
       (44, 35, 1)
       (60, 35, 1)
       (62, 35, 1)
       (67, 35, 1)
       (31, 36, 1)
       (35, 36, 1)
       (36, 36, 1)
       (41, 36, 1)
       (61, 36, 1)
       (12, 37, 1)
       (14, 37, 1)
       (32, 37, 1)
       (37, 37, 1)
       (39, 37, 1)
       (42, 37, 1)
       (62, 37, 1)
       (33, 38, 1)
       (35, 38, 1)
       (38, 38, 1)
       (43, 38, 1)
       (63, 38, 1)
       (30, 39, 1)
       (35, 39, 1)
       (37, 39, 1)
       (62, 39, 1)
       (35, 40, 1)
       (40, 40, 1)
       (41, 40, 1)
       (42, 40, 1)
       (43, 40, 1)
       (44, 40, 1)
       (47, 40, 1)
       (49, 40, 1)
       (65, 40, 1)
       (67, 40, 1)
       (72, 40, 1)
       (36, 41, 1)
       (40, 41, 1)
       (41, 41, 1)
       (66, 41, 1)
       (17, 42, 1)
       (19, 42, 1)
       (37, 42, 1)
       (42, 42, 1)
       (44, 42, 1)
       (47, 42, 1)
       (67, 42, 1)
       (38, 43, 1)
       (40, 43, 1)
       (43, 43, 1)
       (68, 43, 1)
       (35, 44, 1)
       (40, 44, 1)
       (42, 44, 1)
       (67, 44, 1)
       (40, 45, 1)
       (45, 45, 1)
       (47, 45, 1)
       (49, 45, 1)
       (72, 45, 1)
       (41, 46, 1)
       (46, 46, 1)
       (22, 47, 1)
       (24, 47, 1)
       (42, 47, 1)
       (47, 47, 1)
       (49, 47, 1)
       (50, 47, 1)
       (72, 47, 1)
       (43, 48, 1)
       (48, 48, 1)
       (40, 49, 1)
       (47, 49, 1)
       (72, 49, 1)
       (25, 50, 1)
       (47, 50, 1)
       (50, 50, 1)
       (51, 51, 1)
       (55, 51, 1)
       (57, 51, 1)
       (59, 51, 1)
       (82, 51, 1)
       (52, 52, 1)
       (56, 52, 1)
       (53, 53, 1)
       (57, 53, 1)
       (54, 54, 1)
       (58, 54, 1)
       (30, 55, 1)
       (55, 55, 1)
       (56, 55, 1)
       (57, 55, 1)
       (58, 55, 1)
       (59, 55, 1)
       (60, 55, 1)
       (62, 55, 1)
       (64, 55, 1)
       (80, 55, 1)
       (82, 55, 1)
       (87, 55, 1)
       (31, 56, 1)
       (52, 56, 1)
       (55, 56, 1)
       (56, 56, 1)
       (61, 56, 1)
       (81, 56, 1)
       (32, 57, 1)
       (34, 57, 1)
       (53, 57, 1)
       (57, 57, 1)
       (59, 57, 1)
       (62, 57, 1)
       (82, 57, 1)
       (33, 58, 1)
       (54, 58, 1)
       (55, 58, 1)
       (58, 58, 1)
       (63, 58, 1)
       (83, 58, 1)
       (55, 59, 1)
       (57, 59, 1)
       (82, 59, 1)
       (35, 60, 1)
       (55, 60, 1)
       (60, 60, 1)
       (61, 60, 1)
       (62, 60, 1)
       (63, 60, 1)
       (64, 60, 1)
       (65, 60, 1)
       (67, 60, 1)
       (69, 60, 1)
       (85, 60, 1)
       (87, 60, 1)
       (92, 60, 1)
       (36, 61, 1)
       (56, 61, 1)
       (60, 61, 1)
       (61, 61, 1)
       (66, 61, 1)
       (86, 61, 1)
       (37, 62, 1)
       (39, 62, 1)
       (57, 62, 1)
       (62, 62, 1)
       (64, 62, 1)
       (67, 62, 1)
       (87, 62, 1)
       (38, 63, 1)
       (58, 63, 1)
       (60, 63, 1)
       (63, 63, 1)
       (68, 63, 1)
       (88, 63, 1)
       (55, 64, 1)
       (60, 64, 1)
       (62, 64, 1)
       (87, 64, 1)
       (40, 65, 1)
       (60, 65, 1)
       (65, 65, 1)
       (66, 65, 1)
       (67, 65, 1)
       (68, 65, 1)
       (69, 65, 1)
       (72, 65, 1)
       (74, 65, 1)
       (90, 65, 1)
       (92, 65, 1)
       (97, 65, 1)
       (41, 66, 1)
       (61, 66, 1)
       (65, 66, 1)
       (66, 66, 1)
       (91, 66, 1)
       (42, 67, 1)
       (44, 67, 1)
       (62, 67, 1)
       (67, 67, 1)
       (69, 67, 1)
       (72, 67, 1)
       (92, 67, 1)
       (43, 68, 1)
       (63, 68, 1)
       (65, 68, 1)
       (68, 68, 1)
       (93, 68, 1)
       (60, 69, 1)
       (65, 69, 1)
       (67, 69, 1)
       (92, 69, 1)
       (65, 70, 1)
       (70, 70, 1)
       (72, 70, 1)
       (74, 70, 1)
       (97, 70, 1)
       (66, 71, 1)
       (71, 71, 1)
       (47, 72, 1)
       (49, 72, 1)
       (67, 72, 1)
       (72, 72, 1)
       (74, 72, 1)
       (75, 72, 1)
       (97, 72, 1)
       (68, 73, 1)
       (73, 73, 1)
       (65, 74, 1)
       (72, 74, 1)
       (97, 74, 1)
       (72, 75, 1)
       (75, 75, 1)
       (76, 76, 1)
       (80, 76, 1)
       (82, 76, 1)
       (84, 76, 1)
       (107, 76, 1)
       (77, 77, 1)
       (81, 77, 1)
       (78, 78, 1)
       (82, 78, 1)
       (79, 79, 1)
       (83, 79, 1)
       (55, 80, 1)
       (80, 80, 1)
       (81, 80, 1)
       (82, 80, 1)
       (83, 80, 1)
       (84, 80, 1)
       (85, 80, 1)
       (87, 80, 1)
       (89, 80, 1)
       (107, 80, 1)
       (112, 80, 1)
       (56, 81, 1)
       (77, 81, 1)
       (80, 81, 1)
       (81, 81, 1)
       (86, 81, 1)
       (57, 82, 1)
       (59, 82, 1)
       (78, 82, 1)
       (82, 82, 1)
       (84, 82, 1)
       (87, 82, 1)
       (107, 82, 1)
       (58, 83, 1)
       (79, 83, 1)
       (80, 83, 1)
       (83, 83, 1)
       (88, 83, 1)
       (80, 84, 1)
       (82, 84, 1)
       (107, 84, 1)
       (60, 85, 1)
       (80, 85, 1)
       (85, 85, 1)
       (86, 85, 1)
       (87, 85, 1)
       (88, 85, 1)
       (89, 85, 1)
       (90, 85, 1)
       (92, 85, 1)
       (94, 85, 1)
       (112, 85, 1)
       (117, 85, 1)
       (61, 86, 1)
       (81, 86, 1)
       (85, 86, 1)
       (86, 86, 1)
       (91, 86, 1)
       (62, 87, 1)
       (64, 87, 1)
       (82, 87, 1)
       (87, 87, 1)
       (89, 87, 1)
       (92, 87, 1)
       (112, 87, 1)
       (63, 88, 1)
       (83, 88, 1)
       (85, 88, 1)
       (88, 88, 1)
       (93, 88, 1)
       (80, 89, 1)
       (85, 89, 1)
       (87, 89, 1)
       (112, 89, 1)
       (65, 90, 1)
       (85, 90, 1)
       (90, 90, 1)
       (91, 90, 1)
       (92, 90, 1)
       (93, 90, 1)
       (94, 90, 1)
       (97, 90, 1)
       (99, 90, 1)
       (117, 90, 1)
       (122, 90, 1)
       (66, 91, 1)
       (86, 91, 1)
       (90, 91, 1)
       (91, 91, 1)
       (67, 92, 1)
       (69, 92, 1)
       (87, 92, 1)
       (92, 92, 1)
       (94, 92, 1)
       (97, 92, 1)
       (117, 92, 1)
       (68, 93, 1)
       (88, 93, 1)
       (90, 93, 1)
       (93, 93, 1)
       (85, 94, 1)
       (90, 94, 1)
       (92, 94, 1)
       (117, 94, 1)
       (90, 95, 1)
       (95, 95, 1)
       (97, 95, 1)
       (99, 95, 1)
       (122, 95, 1)
       (91, 96, 1)
       (96, 96, 1)
       (72, 97, 1)
       (74, 97, 1)
       (92, 97, 1)
       (97, 97, 1)
       (99, 97, 1)
       (100, 97, 1)
       (122, 97, 1)
       (93, 98, 1)
       (98, 98, 1)
       (90, 99, 1)
       (97, 99, 1)
       (122, 99, 1)
       (97, 100, 1)
       (100, 100, 1)
       (101, 101, 1)
       (107, 101, 1)
       (109, 101, 1)
       (102, 102, 1)
       (103, 103, 1)
       (107, 103, 1)
       (126, 103, 1)
       (104, 104, 1)
       (80, 105, 1)
       (105, 105, 1)
       (107, 105, 1)
       (109, 105, 1)
       (112, 105, 1)
       (114, 105, 1)
       (81, 106, 1)
       (106, 106, 1)
       (82, 107, 1)
       (84, 107, 1)
       (103, 107, 1)
       (107, 107, 1)
       (109, 107, 1)
       (112, 107, 1)
       (127, 107, 1)
       (83, 108, 1)
       (108, 108, 1)
       (107, 109, 1)
       (85, 110, 1)
       (110, 110, 1)
       (112, 110, 1)
       (114, 110, 1)
       (117, 110, 1)
       (119, 110, 1)
       (86, 111, 1)
       (111, 111, 1)
       (87, 112, 1)
       (89, 112, 1)
       (107, 112, 1)
       (112, 112, 1)
       (114, 112, 1)
       (117, 112, 1)
       (128, 112, 1)
       (88, 113, 1)
       (113, 113, 1)
       (112, 114, 1)
       (90, 115, 1)
       (115, 115, 1)
       (117, 115, 1)
       (119, 115, 1)
       (122, 115, 1)
       (91, 116, 1)
       (116, 116, 1)
       (92, 117, 1)
       (94, 117, 1)
       (112, 117, 1)
       (117, 117, 1)
       (119, 117, 1)
       (122, 117, 1)
       (129, 117, 1)
       (93, 118, 1)
       (118, 118, 1)
       (117, 119, 1)
       (120, 120, 1)
       (122, 120, 1)
       (121, 121, 1)
       (97, 122, 1)
       (99, 122, 1)
       (117, 122, 1)
       (122, 122, 1)
       (125, 122, 1)
       (130, 122, 1)
       (123, 123, 1)
       (122, 124, 1)
       (124, 124, 1)
       (122, 125, 1)
       (125, 125, 1)
       (131, 125, 1)
       (126, 126, 1)
       (107, 127, 1)
       (109, 127, 1)
       (127, 127, 1)
       (112, 128, 1)
       (114, 128, 1)
       (128, 128, 1)
       (117, 129, 1)
       (119, 129, 1)
       (129, 129, 1)
       (122, 130, 1)
       (130, 130, 1)
       (131, 131, 1) |]


// Matrix from SuiteSparse Matrix Collection
// link: https://sparse.tamu.edu/Bai/tub100
// name: tub100
// size: 100x100
let testMatrix3Arr =
    [| (1, 1, true)
       (2, 1, true)
       (3, 1, true)
       (1, 2, true)
       (2, 2, true)
       (4, 2, true)
       (1, 3, true)
       (3, 3, true)
       (4, 3, true)
       (5, 3, true)
       (2, 4, true)
       (3, 4, true)
       (4, 4, true)
       (6, 4, true)
       (3, 5, true)
       (5, 5, true)
       (6, 5, true)
       (7, 5, true)
       (4, 6, true)
       (5, 6, true)
       (6, 6, true)
       (8, 6, true)
       (5, 7, true)
       (7, 7, true)
       (8, 7, true)
       (9, 7, true)
       (6, 8, true)
       (7, 8, true)
       (8, 8, true)
       (10, 8, true)
       (7, 9, true)
       (9, 9, true)
       (10, 9, true)
       (11, 9, true)
       (8, 10, true)
       (9, 10, true)
       (10, 10, true)
       (12, 10, true)
       (9, 11, true)
       (11, 11, true)
       (12, 11, true)
       (13, 11, true)
       (10, 12, true)
       (11, 12, true)
       (12, 12, true)
       (14, 12, true)
       (11, 13, true)
       (13, 13, true)
       (14, 13, true)
       (15, 13, true)
       (12, 14, true)
       (13, 14, true)
       (14, 14, true)
       (16, 14, true)
       (13, 15, true)
       (15, 15, true)
       (16, 15, true)
       (17, 15, true)
       (14, 16, true)
       (15, 16, true)
       (16, 16, true)
       (18, 16, true)
       (15, 17, true)
       (17, 17, true)
       (18, 17, true)
       (19, 17, true)
       (16, 18, true)
       (17, 18, true)
       (18, 18, true)
       (20, 18, true)
       (17, 19, true)
       (19, 19, true)
       (20, 19, true)
       (21, 19, true)
       (18, 20, true)
       (19, 20, true)
       (20, 20, true)
       (22, 20, true)
       (19, 21, true)
       (21, 21, true)
       (22, 21, true)
       (23, 21, true)
       (20, 22, true)
       (21, 22, true)
       (22, 22, true)
       (24, 22, true)
       (21, 23, true)
       (23, 23, true)
       (24, 23, true)
       (25, 23, true)
       (22, 24, true)
       (23, 24, true)
       (24, 24, true)
       (26, 24, true)
       (23, 25, true)
       (25, 25, true)
       (26, 25, true)
       (27, 25, true)
       (24, 26, true)
       (25, 26, true)
       (26, 26, true)
       (28, 26, true)
       (25, 27, true)
       (27, 27, true)
       (28, 27, true)
       (29, 27, true)
       (26, 28, true)
       (27, 28, true)
       (28, 28, true)
       (30, 28, true)
       (27, 29, true)
       (29, 29, true)
       (30, 29, true)
       (31, 29, true)
       (28, 30, true)
       (29, 30, true)
       (30, 30, true)
       (32, 30, true)
       (29, 31, true)
       (31, 31, true)
       (32, 31, true)
       (33, 31, true)
       (30, 32, true)
       (31, 32, true)
       (32, 32, true)
       (34, 32, true)
       (31, 33, true)
       (33, 33, true)
       (34, 33, true)
       (35, 33, true)
       (32, 34, true)
       (33, 34, true)
       (34, 34, true)
       (36, 34, true)
       (33, 35, true)
       (35, 35, true)
       (36, 35, true)
       (37, 35, true)
       (34, 36, true)
       (35, 36, true)
       (36, 36, true)
       (38, 36, true)
       (35, 37, true)
       (37, 37, true)
       (38, 37, true)
       (39, 37, true)
       (36, 38, true)
       (37, 38, true)
       (38, 38, true)
       (40, 38, true)
       (37, 39, true)
       (39, 39, true)
       (40, 39, true)
       (41, 39, true)
       (38, 40, true)
       (39, 40, true)
       (40, 40, true)
       (42, 40, true)
       (39, 41, true)
       (41, 41, true)
       (42, 41, true)
       (43, 41, true)
       (40, 42, true)
       (41, 42, true)
       (42, 42, true)
       (44, 42, true)
       (41, 43, true)
       (43, 43, true)
       (44, 43, true)
       (45, 43, true)
       (42, 44, true)
       (43, 44, true)
       (44, 44, true)
       (46, 44, true)
       (43, 45, true)
       (45, 45, true)
       (46, 45, true)
       (47, 45, true)
       (44, 46, true)
       (45, 46, true)
       (46, 46, true)
       (48, 46, true)
       (45, 47, true)
       (47, 47, true)
       (48, 47, true)
       (49, 47, true)
       (46, 48, true)
       (47, 48, true)
       (48, 48, true)
       (50, 48, true)
       (47, 49, true)
       (49, 49, true)
       (50, 49, true)
       (51, 49, true)
       (48, 50, true)
       (49, 50, true)
       (50, 50, true)
       (52, 50, true)
       (49, 51, true)
       (51, 51, true)
       (52, 51, true)
       (53, 51, true)
       (50, 52, true)
       (51, 52, true)
       (52, 52, true)
       (54, 52, true)
       (51, 53, true)
       (53, 53, true)
       (54, 53, true)
       (55, 53, true)
       (52, 54, true)
       (53, 54, true)
       (54, 54, true)
       (56, 54, true)
       (53, 55, true)
       (55, 55, true)
       (56, 55, true)
       (57, 55, true)
       (54, 56, true)
       (55, 56, true)
       (56, 56, true)
       (58, 56, true)
       (55, 57, true)
       (57, 57, true)
       (58, 57, true)
       (59, 57, true)
       (56, 58, true)
       (57, 58, true)
       (58, 58, true)
       (60, 58, true)
       (57, 59, true)
       (59, 59, true)
       (60, 59, true)
       (61, 59, true)
       (58, 60, true)
       (59, 60, true)
       (60, 60, true)
       (62, 60, true)
       (59, 61, true)
       (61, 61, true)
       (62, 61, true)
       (63, 61, true)
       (60, 62, true)
       (61, 62, true)
       (62, 62, true)
       (64, 62, true)
       (61, 63, true)
       (63, 63, true)
       (64, 63, true)
       (65, 63, true)
       (62, 64, true)
       (63, 64, true)
       (64, 64, true)
       (66, 64, true)
       (63, 65, true)
       (65, 65, true)
       (66, 65, true)
       (67, 65, true)
       (64, 66, true)
       (65, 66, true)
       (66, 66, true)
       (68, 66, true)
       (65, 67, true)
       (67, 67, true)
       (68, 67, true)
       (69, 67, true)
       (66, 68, true)
       (67, 68, true)
       (68, 68, true)
       (70, 68, true)
       (67, 69, true)
       (69, 69, true)
       (70, 69, true)
       (71, 69, true)
       (68, 70, true)
       (69, 70, true)
       (70, 70, true)
       (72, 70, true)
       (69, 71, true)
       (71, 71, true)
       (72, 71, true)
       (73, 71, true)
       (70, 72, true)
       (71, 72, true)
       (72, 72, true)
       (74, 72, true)
       (71, 73, true)
       (73, 73, true)
       (74, 73, true)
       (75, 73, true)
       (72, 74, true)
       (73, 74, true)
       (74, 74, true)
       (76, 74, true)
       (73, 75, true)
       (75, 75, true)
       (76, 75, true)
       (77, 75, true)
       (74, 76, true)
       (75, 76, true)
       (76, 76, true)
       (78, 76, true)
       (75, 77, true)
       (77, 77, true)
       (78, 77, true)
       (79, 77, true)
       (76, 78, true)
       (77, 78, true)
       (78, 78, true)
       (80, 78, true)
       (77, 79, true)
       (79, 79, true)
       (80, 79, true)
       (81, 79, true)
       (78, 80, true)
       (79, 80, true)
       (80, 80, true)
       (82, 80, true)
       (79, 81, true)
       (81, 81, true)
       (82, 81, true)
       (83, 81, true)
       (80, 82, true)
       (81, 82, true)
       (82, 82, true)
       (84, 82, true)
       (81, 83, true)
       (83, 83, true)
       (84, 83, true)
       (85, 83, true)
       (82, 84, true)
       (83, 84, true)
       (84, 84, true)
       (86, 84, true)
       (83, 85, true)
       (85, 85, true)
       (86, 85, true)
       (87, 85, true)
       (84, 86, true)
       (85, 86, true)
       (86, 86, true)
       (88, 86, true)
       (85, 87, true)
       (87, 87, true)
       (88, 87, true)
       (89, 87, true)
       (86, 88, true)
       (87, 88, true)
       (88, 88, true)
       (90, 88, true)
       (87, 89, true)
       (89, 89, true)
       (90, 89, true)
       (91, 89, true)
       (88, 90, true)
       (89, 90, true)
       (90, 90, true)
       (92, 90, true)
       (89, 91, true)
       (91, 91, true)
       (92, 91, true)
       (93, 91, true)
       (90, 92, true)
       (91, 92, true)
       (92, 92, true)
       (94, 92, true)
       (91, 93, true)
       (93, 93, true)
       (94, 93, true)
       (95, 93, true)
       (92, 94, true)
       (93, 94, true)
       (94, 94, true)
       (96, 94, true)
       (93, 95, true)
       (95, 95, true)
       (96, 95, true)
       (97, 95, true)
       (94, 96, true)
       (95, 96, true)
       (96, 96, true)
       (98, 96, true)
       (95, 97, true)
       (97, 97, true)
       (98, 97, true)
       (99, 97, true)
       (96, 98, true)
       (97, 98, true)
       (98, 98, true)
       (100, 98, true)
       (97, 99, true)
       (99, 99, true)
       (100, 99, true)
       (98, 100, true)
       (99, 100, true)
       (100, 100, true) |]

let testMatrix1 = Array.toList testMatrix1Arr
let testMatrix2 = Array.toList testMatrix2Arr
let testMatrix3 = Array.toList testMatrix3Arr