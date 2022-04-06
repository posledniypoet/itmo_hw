<template>
    <footer>
      <div class="header">
        Количество пользователей: {{usersCount}}
        Количество постов: {{postsCount}}
      </div>
      <div class="body">
        <a href="#">Codeforces</a> &copy; 2010-2019 by Mike Mirzayanov
      </div>
    </footer>
</template>

<script>
export default {
    name: "Footer",
    props: ["usersCount", "postsCount"]
}
</script>

<style scoped>

</style>