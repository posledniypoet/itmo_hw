<template>
  <div class="middle">
    <Sidebar :posts="posts" :users="users"/>
    <main>
      <Index :users="users" :posts="posts" :comments="comments" v-if="page === 'Index'"/>
      <Enter v-if="page === 'Enter'"/>
      <Register v-if="page === 'Register'"/>
      <WritePost v-if="page === 'WritePost'"/>
      <EditPost v-if="page === 'EditPost'"/>
      <Users :users="users" v-if="page === 'Users'"></Users>
    </main>
  </div>
</template>
<script>
import Index from './middle/Index';
import Enter from './middle/Enter';
import EditPost from "./middle/EditPost";
import Sidebar from "./sidebar/Sidebar";
import WritePost from "./middle/WritePost";
import Users from "./middle/Users";
import Register from "./middle/Register"

export default {
  name: "Middle",
  props: ['users', 'posts', 'comments'],
  data: function () {
    return {
      page: "Index",
      id: null
    }
  },
  components: {
    WritePost,
    EditPost,
    Index,
    Enter,
    Sidebar,
    Users,
    Register
  }, beforeCreate() {
    this.$root.$on("onChangePage", (page, id) => {
      this.page = page;
      this.id = id;
    });
  },
  computed: {
    viewPosts: function () {
      return Object.values(this.posts).sort((a, b) => b.id - a.id).slice(0, 2);
    }
  }
}
</script>

<style scoped>
</style>
