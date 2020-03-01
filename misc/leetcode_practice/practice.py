class Solution:
    def sortLists(self, l1, l2):
        def recursively(result, l1, l2):
            if l1 == []:
                result += l2
                return result
            elif l2 == []:
                result += l1
                return result
            elif l1[0]<l2[0]:
                result.append(l1[0])
                return recursively(result, l1[1:], l2)
            elif l2[0]<l1[0]:
                result.append(l2[0])
                return recursively(result, l1, l2[1:])
            else:
                result.append(l1[0])
                result.append(l2[0])
                return recursively(result, l1[1:], l2[1:])
        return recursively([], l1, l2)

    def findMedianSortedArrays(self, nums1, nums2):
        sorted_nums = self.sortLists(nums1, nums2)
        lngth = len(sorted_nums);
        mid = int(lngth/2)
        if (lngth % 2 == 1):
            return sorted_nums[mid]
        else:
            return (sorted_nums[mid-1] + sorted_nums[mid])/2






